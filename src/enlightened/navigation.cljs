(ns enlightened.navigation
  (:require [enlightened.core :as core]
            [enlightened.widgets :as widgets]))

(declare create-pane)

(defn into-hierarchy [hierarchy nav-items]
  (reduce (fn [m x] (assoc m (keyword (first x)) (rest x)))
          hierarchy nav-items))

(defn create-hierarchy [root-item nav-items]
  (into-hierarchy {:root root-item} nav-items))

(defn toggle-back-binds [widget toggle listener]
  (if toggle
    (doto widget (.onceKey "h" listener) (.onceKey "left" listener))
    (doto widget (.unkey "h" listener) (.unkey "left" listener))))

(defn create-go-next-fn [view current dispatch back widget-hooks]
  (fn [nav-item position hierarchy]
    (toggle-back-binds view false back)
    (create-pane nav-item (assoc-in hierarchy
                                    [:links (first nav-item)]
                                    {:nav-item current :position position})
                 view dispatch widget-hooks)))

(defn clean-widget-hooks [widget-hooks]
  (doto widget-hooks
    (dissoc :back)))

(defn create-pane
  ([current hierarchy view]
     (create-pane current hierarchy view identity []))
  ([current hierarchy view dispatch]
     (create-pane current hierarchy view dispatch []))
  ([[title options :as current] hierarchy view dispatch widget-hooks]
     (let [hooks (clean-widget-hooks widget-hooks)
           label (core/set-title view title)
           items (core/set-items view (take-nth 2 options))
           back (if-let [parent (get-in hierarchy [:links title])]
                  (fn []
                    (toggle-back-binds view false (js* "this"))
                    (create-pane (:nav-item parent)
                                 hierarchy view dispatch
                                 (assoc hooks
                                   :remove-title #(.detach label)
                                   :back #(.select % (:position parent)))))
                  (constantly nil))
           go-next (create-go-next-fn
                    view current dispatch back
                    (assoc hooks :remove-title #(.detach label)))
           go-key (fn [k pos] (if-let [next (get hierarchy k)]
                                (go-next next pos hierarchy)))]
       (toggle-back-binds view true back)
       (.removeAllListeners view "select")
       (.on view "select"
            (fn [_ i]
              (let [action (nth options (inc (* 2 i)))]
                (condp apply [action]
                  fn? (let [res (action)]
                        (condp apply [res]
                          vector? (go-next (-> res first rest) i
                                           (into-hierarchy hierarchy res))
                          keyword? (go-key res i)
                          string? (do (core/set-items
                                       view (update-in items [i]
                                                       #(str % ": " res)))
                                      (core/render-deferred))
                          res))
                  keyword? (go-key action i)
                  (dispatch action)))))
       (doseq [[handle hook] hooks] (hook view))
       (core/render)
       view)))

(defn navigation
  ([nav-items action-dispatch widget-hooks]
     (navigation nav-items action-dispatch widget-hooks (widgets/create :list)))
  ([nav-items action-dispatch widget-hooks list]
     (let [{navs :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-items)]
       (create-pane (first root)
                    (create-hierarchy root navs)
                    list action-dispatch widget-hooks))))

(defn navigation-view [nav-items action-dispatch widget-hooks]
  (navigation nav-items action-dispatch widget-hooks (widgets/list-view)))
