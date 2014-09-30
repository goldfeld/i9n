(ns i9n.core)

#_(defmacro n [arglist & body]
  (let [{plainargs :p otherargs :o}
        (group-by #(if (symbol? %) :p :o) `[~@arglist])
        destructargs (->> (reduce (fn [darglist a]
                                    (condp apply [a]
                                      vector? (conj darglist
                                                    [a (keyword (last a))])
                                      map? (conj darglist
                                                 [a (-> a last last keyword)])
                                      darglist))
                                  [] `~otherargs)
                          (mapcat (fn [[dstrct kw]] [dstrct kw])))
        args (->> (mapcat (fn [a] [a (keyword a)]) `~plainargs)
                  (concat destructargs))
        m (apply hash-map `~args)]
    `{:i9n-action :n-fn 
      :requested-args (take-nth 2 (rest ~args))
      :action
      (fn [~m] ~@body)}))

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
