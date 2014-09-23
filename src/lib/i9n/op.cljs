(ns i9n.op
  (:require [i9n.nav-entry :as nav-entry]
            [i9n.ext :refer [custom-i9n-op]]))

(defmethod custom-i9n-op :add [[cmd & args] nav more]
  (nav-entry/add-to-hierarchy nav args))

(defmethod custom-i9n-op :stub [[cmd & args] nav more]
  (nav-entry/add-to-hierarchy
   nav args #(assoc-in %1 [:hierarchy %2 :dirty] true)))

(defn select-option
  ([widget render! nav i relative-fn]
     (select-option widget render! nav (relative-fn (:pos nav) i)))
  ([widget render! {last :last :as nav} i]
     (if-let [k (cond (= :last i) last
                      (integer? i) (cond (> i last) last
                                         (< i 0) 0
                                         :else i))]
       (let [k' k]
         (.select widget k')
         (render!)
         (assoc nav :pos k'))
       nav)))

(defmethod custom-i9n-op :select [[cmd & args] nav {:keys [widget render!]}]
  (let [target (if (= 1 (count args)) args (reverse args))]
    (apply select-option widget render! nav target)))

(defmethod custom-i9n-op :dirty [[cmd & args] nav more]
  (reduce (fn [n id] (assoc-in nav [:hierarchy id :dirty] true))
          nav args))
