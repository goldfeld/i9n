(ns enlightened.widgets.menu-impl
  (:require [enlightened.core :refer [render]]))

(defn create-hierarchy [root-item menu-items]
  (let [hierarchy {:root root-item}]
    (reduce (fn [coll x] (conj coll [(keyword (first x)) (rest x)]))
            hierarchy menu-items)))

(defn create-menu
  ([current hierarchy view-impl]
     (create-menu current hierarchy view-impl identity []))
  ([current hierarchy view-impl dispatch]
     (create-menu current hierarchy view-impl dispatch []))
  ([current hierarchy view-impl dispatch widget-hooks]
     (let [[title options] current
           menu (view-impl title (take-nth 2 options)
                           (fn [i items]
                             (let [action (nth options (inc (* 2 i)))
                                   next (action hierarchy)
                                   hrchy (assoc-in hierarchy
                                                   [:links (first next)]
                                                   {:menu-item current
                                                    :position i})]
                               (condp apply [action]
                                 fn? (action)
                                 keyword? (create-menu next hrchy view-impl
                                                       dispatch widget-hooks)
                                 (dispatch action)))))]
       (let [back (if-let [parent (get-in hierarchy [:links title])]
                    #(create-menu (:menu-item parent)
                                  hierarchy view-impl dispatch
                                  (conj widget-hooks
                                        (fn [menu]
                                          (.select menu (:position parent))
                                          (render))))
                    (constantly nil))]
         (doto menu
           (.onceKey "h" back)
           (.onceKey "left" back)))
       (doseq [hook widget-hooks] (hook menu))
       menu)))
