(ns enlightened.nav-entry
  "Pure, testable helpers for enlightnened.navigation")

(defn create-hierarchy [root-item nav-entries]
  (reduce (fn [m x] (assoc m (keyword (first x)) (vec (rest x))))
          {:root root-item} nav-entries))
