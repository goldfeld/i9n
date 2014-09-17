(ns i9n.more)

(defn index-of
  ([coll item] (.indexOf (clj->js coll) (clj->js item)))
  ([coll item & items]
     (let [jscoll (clj->js coll)]
       (some #(let [i (.indexOf jscoll %)] (and (not= -1 i) i))
             (clj->js (cons item items))))))

(defn splice [coll idx cnt & items]
  (let [[a b] (split-at idx coll)]
    (concat a items (drop cnt b))))

(defn replace-at-indexes
  "Replaces elements at coll at the given indexes with the given
  replacements, and coll must be a vector. If indexes and replacements
  don't have the same length, replacement will stop as soon as the
  shorter one is exhausted."
  [coll indexes replacements]
  (if (and (seq indexes) (seq replacements))
    (replace-at-indexes (assoc coll (first indexes) (first replacements))
                        (rest indexes) (rest replacements))
    coll))

(defn channel? [x]
  (satisfies? cljs.core.async.impl.protocols/Channel x))

(defn route->keyword [s]
  (->> (clojure.string/split s #"[\\/]" 2)
       (map #(clojure.string/replace % "%20" "_+_"))
       (apply keyword)))

(defn encode-query-params [params]
  (if (seq params)
    (-> (reduce-kv (fn [s k v] (str s "&" (name k) "=" v)) (str) params)
        (subs 1)
        (#(str "?" %)))
    (str)))

(defn encode-keyword
  ([s] (encode-keyword s {}))
  ([s query-params]
     (->> (clojure.string/split (str s (encode-query-params query-params))
                                #"[\\/]" 2)
          (map #(clojure.string/replace % #" " "_+_"))
          (apply keyword))))

(defn decode-keyword [k]
  (clojure.string/replace (subs (str k) 1) "_+_" " "))
