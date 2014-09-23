(ns i9n.op.state
  (:require [i9n.nav-entry :as nav-entry]
            [i9n.ext :refer [custom-i9n-op]]))

(defn update-state [nav state-id state-val]
  (if-let [{:keys [set deps]} (get-in nav [:state state-id])]
    (update-in nav [:state state-id :val]
               (fn [old-val]
                 (set state-val old-val 
                      (select-keys (:state nav) deps))))
    (assoc-in nav [:state state-id]
              {:val state-val
               :get (fn [val] val)
               :set (fn [val _ _] val)})))

(defn update-states [nav & id-state-pairs]
  (reduce (fn [n [state-id state-val]]
            (if-let [dpdnts (get-in n [:state state-id :dependents])]
              (reduce (fn [n' dpdt]
                        (condp apply [dpdt]
                          fn? (do (dpdt) n')
                          keyword? (let [d (get-in n' [:state dpdt])]
                                     (or (and (map? d) (= :eager (:computed d))
                                              (update-state n' dpdt (:val d)))
                                         n'))))
                      (update-state n state-id state-val)
                      dpdnts)
              (update-state n state-id state-val)))
          nav (partition 2 id-state-pairs)))

(defmethod custom-i9n-op :state [[cmd & args] nav more]
  (apply update-states nav args))
