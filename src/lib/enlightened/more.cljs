(ns enlightened.more)

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
