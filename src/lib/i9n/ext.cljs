(ns i9n.ext)

(defmulti custom-i9n (fn [i9n-map more] (:i9n i9n-map)))
(defmethod custom-i9n :default [i9n-map more] nil)
