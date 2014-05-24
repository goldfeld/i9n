(ns enlightened.widgets.menu)

(defn create-menu-hierarchy [root-item menu-items]
  (let [hierarchy {:root root-item}]
    (reduce (fn [coll x] (conj coll [(keyword (first x)) (rest x)]))
            hierarchy menu-items)))

(defn create-menu
  ([menu-item hierarchy view-impl]
     (create-menu menu-item hierarchy view-impl identity []))
  ([menu-item hierarchy view-impl action-dispatch]
     (create-menu menu-item hierarchy view-impl action-dispatch []))
  ([menu-item hierarchy view-impl action-dispatch widget-hooks]
     (let [[title options] menu-item
           menu (view-impl title (take-nth 2 options)
                           (fn [i items]
                             (let [selection (nth options (* 2 i))
                                   action (nth options (inc (* 2 i)))]
                               (condp apply [action]
                                 keyword? (create-menu (action hierarchy)
                                                       (assoc-in
                                                        hierarchy
                                                        [:parents selection]
                                                        menu-item)
                                                       view-impl
                                                       action-dispatch
                                                       widget-hooks)
                                 (action-dispatch action)))))]
       (doseq [hook widget-hooks] (hook menu))
       menu)))
