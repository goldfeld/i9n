(ns enlightened.ext)

(defmulti i9n (fn [i9n-map parent nav cfg] (:i9n i9n-map)))
(defmethod i9n :default [i9n-map parent nav cfg] nil)
