(ns i9n.core)

(defmacro n [arglist & body]
  `{:i9n-action :n-fn
    :action (fn ~arglist ~@body)})

(defmacro route [r params & body]
  (let [r' (clojure.string/replace (subs (str r) 1) "?" ":")
        id-symb 'id]
    `{:i9n-step :route
      :route ~r'
      :dispatch
      (fn [{:keys ~params :as m#}]
        (let [id# (i9n.more/route->keyword (secretary.core/render-route ~r' m#))
              ~'id (when (some #{'~id-symb} '~params) id#)
              entry# (do ~@body)]
          (if (= 3 (count entry#)) entry# (apply vector id# entry#))))}))
