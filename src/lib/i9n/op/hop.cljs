(ns i9n.op.hop
  (:require [cljs.core.async :as a]
            [i9n.ext :refer [i9n-op]]
            [i9n.step :refer [route-dispatch!]]
            [i9n.more :as more]
            [i9n.nav-entry :as nav-entry]))

(defn may-flush [nav id chan put!]
  (if (and chan (get-in nav [:hierarchy id :dirty]))
    (do (put! chan id)
        (assoc-in nav [:hierarchy id :dirty] false))
    nav))

(defn may-trigger [nav id in hra]
  (if-let [trigger (get-in nav [:hierarchy id :trigger])]
    (do (hra (trigger id in) nil hra nav)
        (update-in nav [:hierarchy id] dissoc :trigger))
    nav))

(defn hop [[id _ body :as entry] pos nav {:keys [refresh channels] :as more}]
  (-> (if (vector? body) (nav-entry/set-last nav (count body)) nav)
      (may-flush id (:flush channels) (:put! more))
      (may-trigger id (:in channels) (:handle-returned-action more))
      (assoc :pos pos
             :current entry
             :rm-back (:back nav)
             :back (when-let [parent (get-in nav [:hierarchy id :link])]
                     #((:put! more) (:in channels)
                       [:set (:nav-entry parent) (:pos parent)])))
      refresh))

(defn may-hop [target nav {:keys [may-create-link may-abort do-hop]}]
  (condp apply [target]
    vector? (do-hop target (-> (may-create-link nav (first target))
                               (nav-entry/add-to-hierarchy [target])))
    keyword?
    (if-let [dest (get-in nav [:hierarchy target :data])]
      (may-abort target nav #(do-hop (into [target] dest)
                                     (may-create-link nav target)))
      (if-let [routed (route-dispatch! (str "/" (more/decode-keyword target)))]
        (let [n (nav-entry/add-to-hierarchy nav [routed])
              id (first routed)]
          (may-abort id n #(do-hop (into [id] (get-in n [:hierarchy id :data]))
                                   (may-create-link n id))))
        nav))))

(defmethod i9n-op :next [[cmd & args] nav more]
  (let [create-link (fn [n id] (assoc-in n [:hierarchy id :link]
                                         {:nav-entry (:current nav)
                                          :pos (nth args 1 0)}))]
    (may-hop (first args) nav
             {:may-create-link create-link
              :do-hop #(hop %1 (nth args 2 0) %2 more)
              :may-abort
              (fn [id n do-hop]
                (if (not= id (first (:current n))) (do-hop) n))})))

(defmethod i9n-op :hop [[cmd & args] nav more]
  (may-hop (first args) nav
           {:may-create-link (fn [n id] n)
            :do-hop #(hop %1 (nth args 1 0) %2 more)
            :may-abort (fn [id _ do-hop] (do-hop))}))

(defmethod i9n-op :set [[cmd & [nav-entry go-to]] nav more]
  (hop nav-entry (or go-to 0) nav more))
