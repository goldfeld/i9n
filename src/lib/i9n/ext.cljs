(ns i9n.ext)

(defmulti i9n-op (fn [op nav more] (first op)))
(defmethod i9n-op :default [op nav more] nav)

(defmulti custom-i9n (fn [i9n-map more] (:i9n i9n-map)))
(defmethod custom-i9n :default [i9n-map more] nil)
