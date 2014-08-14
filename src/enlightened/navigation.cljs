(ns enlightened.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.core :as core :refer [channel?]]
            [enlightened.widgets :as widgets]))

(declare create-pane)

(defn into-hierarchy [hierarchy nav-entries]
  (reduce (fn [m x] (assoc m (keyword (first x)) (rest x)))
          hierarchy nav-entries))

(defn create-hierarchy [root-item nav-entries]
  (into-hierarchy {:root root-item} nav-entries))

(defn bind-back-handler [widget back-handler]
  (.onceKey widget (clj->js ["h" "left"]) back-handler))

(defn unbind-back-handler [widget back-handler]
  (.unkey widget (clj->js ["h" "left"]) back-handler))

(defn create-nav-fn [widget focus dispatch]
  (fn [next back-handler hierarchy chan hooks]
    (when chan (a/close! chan))
    (unbind-back-handler focus back-handler)
    (.detach (first (.-children widget)))
    (create-pane chan next hierarchy widget dispatch hooks)))

(defn create-back-handler [nav-fn title hierarchy hooks]
  (if-let [parent (get-in hierarchy [:links title])]
    #(nav-fn (:nav-entry parent) (js* "this") hierarchy nil
             (assoc hooks :back (fn [vw]
                                  (.select vw (:pos parent))
                                  :do-once)))
    (constantly nil)))

(defn async-pane [chan go-next widget pos hierarchy]
  (go (let [title (a/<! chan)]
        (a/reduce (fn [options opt]
                    (.detach (first (.-children widget)))
                    (let [items (into options opt)]
                      (go-next [title items] pos hierarchy chan)
                      items))
                  [] (a/partition 2 chan)))))

(defn create-text-viewer [pane text]
  (let [viewer (widgets/text text)]
    (core/set-items pane [])
    (.append pane viewer)
    (.focus viewer)
    viewer))

(defn create-pane
  ([current hierarchy widget]
     (create-pane current hierarchy widget identity {}))
  ([current hierarchy widget dispatch]
     (create-pane current hierarchy widget dispatch {}))
  ([current hierarchy widget dispatch widget-hooks]
     (create-pane nil current hierarchy widget dispatch widget-hooks))
  ([chan [title body :as current] hierarchy widget dispatch widget-hooks]
     (let [[options focus next-hooks]
           (condp apply [body]
             string? (let [txt (create-text-viewer widget body)]
                       [nil txt {:remove-text (fn [] (.detach txt) :do-once)}])
             [body widget {}])
           items (if options (vec (core/set-items widget (take-nth 2 options))))
           hooks (merge (doall (reduce (fn [m [handle hook]]
                                         (let [res (hook widget)]
                                           (when-not (= :do-once res)
                                             (assoc m handle hook))))
                                       {} widget-hooks))
                        next-hooks)
           go-nav (create-nav-fn widget focus dispatch)
           back (create-back-handler go-nav title hierarchy hooks)
           go-next (fn [next pos hrchy chan]
                     (go-nav next back (assoc-in hrchy [:links (first next)]
                                                 {:nav-entry current :pos pos})
                             chan hooks))
           go-key (fn [k pos] (if-let [next (get hierarchy k)]
                                (go-next next pos hierarchy nil)))]
       (bind-back-handler focus back)
       (core/set-title widget title)
       (.removeAllListeners widget "select")
       (.on
        widget "select"
        (fn [_ i]
          (let [action (nth options (inc (* 2 i)))]
            (condp apply [action]
              keyword? (go-key action i)
              channel? (async-pane action go-next widget i hierarchy)
              fn?
              (let [res (action)]
                (condp apply [res]
                  vector? (go-next (-> res first rest) i
                                   (into-hierarchy hierarchy res) nil)
                  keyword? (go-key res i)
                  string? (when items
                            (core/set-items widget
                                            (->> #(str % ": " res)
                                                 (update-in items [i])))
                            (core/render-deferred))
                  channel? (async-pane res go-next widget i hierarchy)
                  (if (.hasOwnProperty res "screen") (.detach widget) res)))
              channel? (async-pane action go-next widget i hierarchy)
              (dispatch action)))))
       (core/render))
     widget))

(defn navigation
  ([nav-entries action-dispatch widget-hooks]
     (navigation nav-entries action-dispatch widget-hooks
                 (widgets/create :list)))
  ([nav-entries action-dispatch widget-hooks list]
     (let [{navs :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-entries)]
       (create-pane nil (first root)
                    (create-hierarchy root navs)
                    list action-dispatch widget-hooks))))

(defn navigation-view [nav-entries action-dispatch widget-hooks]
  (navigation nav-entries action-dispatch widget-hooks (widgets/list-view)))
