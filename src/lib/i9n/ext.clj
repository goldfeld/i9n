(ns i9n.ext)

(defmacro defop [name arglist conditions & body]
  (let [op (keyword name)]
    `(defmethod i9n.ext/custom-i9n-op ~op ~arglist
       ~@body)))
