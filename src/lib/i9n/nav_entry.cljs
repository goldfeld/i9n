(ns i9n.nav-entry
  "Pure, testable helpers for i9n.navigation & co."
  (:require [i9n.ext :as ext]))

(defn add-to-hierarchy
  ([nav nav-entries] (add-to-hierarchy nav nav-entries nil))
  ([nav nav-entries update-in-entry]
     (reduce (fn [n entry]
               (condp apply [entry]
                 vector?
                 (let [[id title body] entry
                       has-trigger (and (vector? body) (odd? (count body)))
                       n' (if has-trigger
                            (assoc-in n [:hierarchy id :trigger] (last body))
                            n)]
                   (-> (if update-in-entry (update-in-entry n' id) n')
                       (assoc-in
                        [:hierarchy id :data]
                        [title (if has-trigger (vec (butlast body)) body)])))
                 map? (if (contains? entry :i9n-step)
                        (ext/custom-i9n-step entry n {})
                        n)
                 n))
             nav nav-entries)))

(defn create-nav [root-item nav-entries]
  (add-to-hierarchy {:hierarchy {:root {:data root-item}}}
                    nav-entries))

(defn fill-body
  "Prepares an entry's body for a fix by filling with nils as
  needed. If body is not a vector, return a vector full of nil
  elements."
  [body place fix-length]
  (if (vector? body)
    (let [elements-to-add (- (+ place fix-length) (count body))]
      (if (> elements-to-add 0)
        (into body (repeat elements-to-add nil))
        body))
    (vec (repeat (+ place fix-length) nil))))

(defn set-last [nav i]
  (assoc nav :last (-> i (/ 2) int dec)))
