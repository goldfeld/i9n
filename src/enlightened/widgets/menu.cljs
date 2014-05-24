(ns enlightened.widgets.menu)

(defn create-menu-hierarchy [root-item menu-items]
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
                                                   [:parents (first next)]
                                                   current)]
                               (condp apply [action]
                                 fn? (action)
                                 keyword? (create-menu next hrchy view-impl
                                                       dispatch widget-hooks)
                                 (dispatch action)))))]
       (let [back (if-let [parent (get-in hierarchy
                                          [:parents title])]
                    #(create-menu parent hierarchy view-impl
                                  dispatch widget-hooks)
                    (constantly nil))]
         (.onceKey menu "h" back)
         (.onceKey menu "left" back))
       (doseq [hook widget-hooks] (hook menu))
       menu)))
