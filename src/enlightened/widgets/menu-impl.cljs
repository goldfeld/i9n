(ns enlightened.widgets.menu-impl
  (:require [enlightened.core :as core]))

(declare create-menu)

(defn into-hierarchy [hierarchy menu-items]
  (reduce (fn [m x] (assoc m (keyword (first x)) (rest x)))
          hierarchy menu-items))

(defn create-hierarchy [root-item menu-items]
  (into-hierarchy {:root root-item} menu-items))

(defn toggle-back-binds [widget toggle listener]
  (if toggle
    (doto widget (.onceKey "h" listener) (.onceKey "left" listener))
    (doto widget (.unkey "h" listener) (.unkey "left" listener))))

(defn create-go-next-fn [view current dispatch back widget-hooks]
  (fn [menu-item position hierarchy]
    (toggle-back-binds view false back)
    (create-menu menu-item (assoc-in hierarchy
                                     [:links (first menu-item)]
                                     {:menu-item current :position position})
                 view dispatch widget-hooks)))

(defn clean-widget-hooks [widget-hooks]
  (doto widget-hooks
    (dissoc :back)))

(defn create-menu
  ([current hierarchy view]
     (create-menu current hierarchy view identity []))
  ([current hierarchy view dispatch]
     (create-menu current hierarchy view dispatch []))
  ([[title options :as current] hierarchy view dispatch widget-hooks]
     (let [hooks (clean-widget-hooks widget-hooks)
           label (core/set-title view title)
           items (core/set-items view (take-nth 2 options))
           back (if-let [parent (get-in hierarchy [:links title])]
                  (fn []
                    (toggle-back-binds view false (js* "this"))
                    (create-menu (:menu-item parent)
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
