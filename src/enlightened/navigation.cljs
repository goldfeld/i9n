(ns enlightened.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.core :as core :refer [channel? widget?]]
            [enlightened.widgets :as widgets]))

(declare create-pane)

(defn create-hierarchy [root-item nav-entries]
  (reduce (fn [m x] (assoc m (keyword (first x)) (rest x)))
          {:root root-item} nav-entries))

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
            book-chan (a/chan)]
        (go (let [res (<! (a/reduce
                           (fn [m msg] (case (first msg)
                                         :pos (assoc m :pos (second msg))
                                         m))
                           {}
                           (go-to-book-part selection book-chan book
                                            (atom nil) binds
                                            (take-nth 2 options) actions)))]
              (.detach book)
              (restore-state (or (:pos res) 0))
              (core/render-deferred)))
        nil)
      (nth actions selection))))

(defn create-selection-fn
  "Creates the pane/navigation selection fn to be passed to blessed's
  list internals. Here is all the logic dependant on what type of se"
  [widget [id title options] chan cfg]
  (fn [_ i]
    (let [action (create-action! options i #(a/put! chan [:hop id %])
                                 widget (:key-binds cfg))]
      (condp apply [action]
        keyword? (a/put! chan [:next action i])
        string? (let [text (create-text-viewer! widget action)]

                  [nil text {:remove-text #(do (.detach text) :do-once)}])

        channel? (a/pipe action chan false)
        fn?
        (let [res (action)]
          (condp apply [res]
            keyword? (a/put! chan [:next res i])
            vector? (doseq [msg [(into [:add] res)
                                 [:next (ffirst res) i]]]
                      (a/put! chan msg))
            string? (let [item (* 2 i)]
                      (a/put! chan [:put 
                                    [id item (str (get options item) ": " res)]
                                    [id (inc item) (constantly nil)]]))
            channel? (a/pipe res chan false)
            nil? nil
            widget? (do (.detach widget) res)))
        ((:dispatch cfg) action)))))

(defn create-refresh-fn [widget chan cfg]
  (fn [nav]
    (let [[id title body :as current] (:current nav)
          options
          (condp apply [body]
            string? (let [text (create-text-viewer! widget body)]
                      (core/set-key-once text (-> cfg :key-binds :left)
                                         #(do (.detach text)
                                              (when-let [bk (:back nav)] (bk))))
                      nil)
            body)
          left-binds (-> cfg :key-binds :left)]
      (when-let [old-title (first (.-children widget))] (.detach old-title))
      (when-let [rm (:rm-back nav)] (core/unset-key widget left-binds rm))
      (when-let [back (:back nav)] (core/set-key-once widget left-binds back))
      (doto widget
        (core/set-title title)
        (.removeAllListeners "select"))
      (when options 
        (doto widget
          (core/set-items (take-nth 2 options))
          (.select (:pos nav))
          (.on "select" (create-selection-fn widget current chan cfg))))
      (core/render))
    (dissoc nav :rm-back)))

(defn change [nav args refresh persist?]
  (let [current-id (-> nav :current first)
        new-nav (reduce
                 (fn [n [id place fix]]
                   (let [title (= :title place)
                         n' (if persist?
                              (assoc-in n (into [:hierarchy id]
                                                (if title [0] [1 place])) fix)
                              n)]
                     (if (= id current-id)
                       (-> (assoc n' :dirty true)
                           (assoc-in (cons :current
                                           (if title [1] [2 place])) fix))
                       n')))
                 nav args)
        n (dissoc new-nav :dirty)]
    (if (:dirty new-nav) (refresh n) n)))

(defn hop [nav-entry pos nav chan refresh]
  (let [id (first nav-entry)]
    (-> (assoc nav
          :pos pos
          :current nav-entry
          :rm-back (:back nav)
          :back (when-let [parent (get-in nav [:hierarchy :links id])]
                  #(a/put! chan [:set (:nav-entry parent) (:pos parent)])))
        refresh)))

(def config-default
  {:dispatch identity
   :key-binds {:quit ["q" "escape"]
               :left ["h" "left"]
               :right ["l" "right"]}})

(defn create-pane
  ([current hierarchy widget]
     (create-pane current hierarchy widget config-default))
  ([[id title body :as current] hierarchy widget cfg]
     (let [chan (or (:chan cfg) (a/chan))
           refresh (create-refresh-fn widget chan cfg)] 
       (a/reduce
        (fn [nav [cmd & args]]
          (case cmd
            :next
            (let [[id current-pos] args
                  dest (get-in nav [:hierarchy id])]
              (if-not dest
                nav
                (hop (into [id] dest) 0
                     (-> (assoc nav :pos current-pos)
                         (assoc-in [:hierarchy :links id]
                                   {:nav-entry (:current nav)
                                    :pos current-pos}))
                     chan refresh)))
            :hop
            (let [[id p] args
                  dest (get-in nav [:hierarchy id])]
              (if-not dest
                nav
                (hop (into [id] dest) (or p 0) nav chan refresh)))
            :set
            (let [[nav-entry p] args] (hop nav-entry p nav chan refresh))
            :add
            (reduce (fn [n [id & more]]
                      (assoc-in n [:hierarchy id] (vec more)))
                    nav args)
            :fix (change nav args refresh :persist)
            :put (change nav args refresh false)))
        {:current current :pos 0 :hierarchy hierarchy}
        chan)
       (a/put! chan [:hop id]))
     widget))

(defn navigation
  ([nav-entries] (navigation nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/create :list)))
  ([nav-entries cfg list]
     (let [{entries :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-entries)]
       (create-pane (cons :root (first root))
                    (create-hierarchy (first root) entries)
                    list (merge config-default cfg)))))

(defn navigation-view
  ([nav-entries] (navigation-view nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/list-view))))
