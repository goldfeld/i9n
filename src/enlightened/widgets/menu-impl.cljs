(ns enlightened.widgets.menu-impl
  (:require [enlightened.core :as core]))

(defn create-hierarchy [root-item menu-items]
  (let [hierarchy {:root root-item}]
    (reduce (fn [coll x] (conj coll [(keyword (first x)) (rest x)]))
            hierarchy menu-items)))

(defn toggle-back-binds [widget toggle listener]
  (if toggle
    (doto widget (.onceKey "h" listener) (.onceKey "left" listener))
    (doto widget (.unkey "h" listener) (.unkey "left" listener))))

(defn create-menu
  ([current hierarchy view]
     (create-menu current hierarchy view identity []))
  ([current hierarchy view dispatch]
     (create-menu current hierarchy view dispatch []))
  ([current hierarchy view dispatch widget-hooks]
     (let [[title options] current
           back (if-let [parent (get-in hierarchy [:links title])]
                  (fn []
                    (toggle-back-binds view false (js* "this"))
                    (create-menu (:menu-item parent)
                                 hierarchy view dispatch
                                 (conj widget-hooks
                                       (fn [view]
                                         (.select view (:position parent))
                                         (core/render)))))
                  (constantly nil))]
       (doto view
         (core/set-items (take-nth 2 options))
         (core/set-title title)
         (toggle-back-binds true back)
         (.on "select"
              (fn [_ i]
                (let [action (nth options (inc (* 2 i)))]
                  (condp apply [action]
                    fn? (action)
                    keyword? (if-let [next (action hierarchy)]
                               (do (toggle-back-binds view false back)
                                   (create-menu next
                                                (assoc-in hierarchy
                                                          [:links (first next)]
                                                          {:menu-item current
                                                           :position i})
                                                view dispatch widget-hooks)))
                    (dispatch action))))))
       (doseq [hook widget-hooks] (hook view))
       (core/render)
       view)))
