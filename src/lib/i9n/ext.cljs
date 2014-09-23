(ns i9n.ext)

(defmulti custom-i9n-op (fn [op nav more] (first op)))
(defmethod custom-i9n-op :default [op nav more] nav)

(defmulti custom-i9n (fn [i9n-map more] (:i9n i9n-map)))
(defmethod custom-i9n :default [i9n-map more] nil)

(defmulti custom-i9n-action (fn [action-map more] (:i9n-action action-map)))
(defmethod custom-i9n-action :default [action-map more] nil)
