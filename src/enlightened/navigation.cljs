(ns enlightened.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.core :as core :refer [channel? widget?]]
            [enlightened.widgets :as widgets]))

(declare create-pane)

(defn into-hierarchy [hierarchy nav-entries]
  (reduce (fn [m x] (assoc m (keyword (first x)) (rest x)))
          hierarchy nav-entries))

(defn create-hierarchy [root-item nav-entries]
  (into-hierarchy {:root root-item} nav-entries))

(defn bind-back-handler [widget back-handler left-binds]
  (core/set-key-once widget left-binds back-handler))

(defn unbind-back-handler [widget back-handler left-binds]
  (core/unset-key widget left-binds back-handler))

(defn create-nav-fn [widget focus cfg]
  (fn [next back-handler hierarchy chan hooks]
    (when chan (a/close! chan))
    (unbind-back-handler focus back-handler (-> cfg :key-binds :left))
    (.detach (first (.-children widget)))
    (create-pane chan next hierarchy widget (assoc cfg :widget-hooks hooks))))

(defn create-back-handler [nav-fn id hierarchy hooks]
  (if-let [parent (get-in hierarchy [:links id])]
    #(nav-fn (:nav-entry parent) (js* "this") hierarchy nil
             (assoc hooks :back (fn [widget]
                                  (.select widget (:pos parent))
                                  :do-once)))
    (constantly nil)))

(defn async-pane [chan go-next widget pos hierarchy]
  (go (let [id (a/<! chan)
            title (a/<! chan)]
        (a/reduce (fn [options opt]
                    (.detach (first (.-children widget)))
                    (let [items (into options opt)]
                      (go-next [id title items] pos hierarchy chan)
                      items))
                  [] (a/partition 2 chan)))))

(defn create-text-viewer!
  ([pane text]
     (let [viewer (create-text-viewer! pane)]
       (.setContent viewer text)
       viewer))
  ([pane]
     (let [viewer (widgets/text)]
       (core/set-items pane [])
       (.append pane viewer)
       (.focus viewer)
       viewer)))

(defn go-to-book-part [i chan book km key-binds titles parts]
  (apply core/unset-keys book @km)
  (.setContent book (nth parts i))
  (core/render)
  (let [quit (fn [] (a/put! chan [:pos i]) (a/close! chan))
        kmaps [(:quit key-binds) quit
               (:left key-binds)
               (if (> i 0)
                 #(go-to-book-part (dec i) chan book km key-binds titles parts)
                 quit)
               (:right key-binds)
               (if (< i (dec (count parts)))
                 #(go-to-book-part (inc i) chan book km key-binds titles parts)
                 (constantly nil))]]
    (reset! km kmaps)
    (apply core/set-keys book kmaps)
    chan))

(defn create-action! [options selection restore-state pane binds]
  (let [actions (take-nth 2 (rest options))]
    (if (every? string? actions)
      (let [book (create-text-viewer! pane)
            chan (a/chan)]
        (go (let [res (<! (a/reduce
                           (fn [m msg] (case (first msg)
                                         :pos (assoc m :pos (second msg))
                                         m))
                           {}
                           (go-to-book-part selection chan book (atom nil) binds
                                            (take-nth 2 options) actions)))]
              (.detach book)
              (restore-state (or (:pos res) 0))
              (core/render-deferred)))
        nil)
      (nth actions selection))))

(def config-default
  {:dispatch identity
   :widget-hooks {}
   :key-binds {:quit ["q" "escape"]
               :left ["h" "left"]
               :right ["l" "right"]}})

(defn create-selection-fn
  "Creates the pane/navigation selection fn to be passed to blessed's
  list internals. Here is all the logic dependant on what type of se"
  [widget options go-next restore hierarchy cfg]
  (let [go-id (fn [id pos] (if-let [next (get hierarchy id)]
                             (go-next (cons id next) pos hierarchy nil)))]
    (fn [_ i]
      (let [action (create-action! options i restore widget (:key-binds cfg))]
        (condp apply [action]
          keyword? (go-id action i)
          string? (let [text (create-text-viewer! widget action)]

                    [nil text {:remove-text #(do (.detach text) :do-once)}])

          channel? (async-pane action go-next widget i hierarchy)
          fn?
          (let [res (action)]
            (condp apply [res]
              vector? (go-next (first res) i (into-hierarchy hierarchy res) nil)
              keyword? (go-id res i)
              string? (when-let [items (take-nth 2 options)]
                        (core/set-items widget
                                        (->> #(str % ": " res)
                                             (update-in (vec items) [i])))
                        (core/render-deferred))
              channel? (async-pane res go-next widget i hierarchy)
              widget? (do (.detach widget) res)))
          channel? (async-pane action go-next widget i hierarchy)
          keyword? (go-id action i)
          ((:dispatch cfg) action))))))

(defn create-pane
  ([current hierarchy widget]
     (create-pane nil current hierarchy widget config-default))
  ([current hierarchy widget cfg]
     (create-pane nil current hierarchy widget cfg))
  ([chan [id title body :as current] hierarchy widget cfg]
     (let [[options focus next-hooks]
           (condp apply [body]
             string? (let [text (create-text-viewer! widget body)]
                       [nil text {:remove-text #(do (.detach text) :do-once)}])
             [body widget {}])
           hooks (merge (doall (reduce (fn [m [handle hook]]
                                         (let [res (hook widget)]
                                           (when-not (= :do-once res)
                                             (assoc m handle hook))))
                                       {} (:widget-hooks cfg)))
                        next-hooks)
           go-nav (create-nav-fn widget focus cfg)
           back (create-back-handler go-nav id hierarchy hooks)
           go-next (fn [next pos hrchy chan]
                     (go-nav next back (assoc-in hrchy [:links (first next)]
                                                 {:nav-entry current
                                                  :pos pos})
                             chan hooks))
           restore (fn [pos]
                     (create-pane chan (cons id (get hierarchy id)) hierarchy
                                  widget (assoc cfg :widget-hooks hooks))
                     (.select widget pos))]
       (when options (core/set-items widget (take-nth 2 options)))
       (bind-back-handler focus back (-> cfg :key-binds :left))
       (doto widget
         (core/set-title title)
         (.removeAllListeners "select")
         (.on "select" (create-selection-fn widget options go-next
                                            restore hierarchy cfg)))
       (core/render))
     widget))

(defn navigation
  ([nav-entries] (navigation nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/create :list)))
  ([nav-entries cfg list]
     (let [{navs :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-entries)]
       (create-pane nil (cons :root (first root))
                    (create-hierarchy root navs)
                    list (merge config-default cfg)))))

(defn navigation-view
  ([nav-entries] (navigation-view nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/list-view))))
