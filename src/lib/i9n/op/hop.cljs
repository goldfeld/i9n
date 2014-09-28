(ns i9n.op.hop
  (:require [cljs.core.async :as a]
            [secretary.core :as secretary]
            [i9n.more :as more]
            [i9n.nav-entry :as nav-entry]))

(defn may-flush [nav id chan]
  (if (and chan (get-in nav [:hierarchy id :dirty]))
    (do (a/put! chan id)
        (assoc-in nav [:hierarchy id :dirty] false))
    nav))

(defn may-trigger [nav id in hra]
  (if-let [trigger (get-in nav [:hierarchy id :trigger])]
    (do (hra (trigger id in) nil hra nav)
        (update-in nav [:hierarchy id] dissoc :trigger))
    nav))

(defn hop [[id _ body :as entry] pos nav {:keys [refresh channels] :as more}]
  (-> (if (vector? body) (nav-entry/set-last nav (count body)) nav)
      (may-flush id (:flush channels))
      (may-trigger id (:in channels) (:handle-returned-action more))
      (assoc :pos pos
             :current entry
             :rm-back (:back nav)
             :back (when-let [parent (get-in nav [:hierarchy id :link])]
                     #(a/put! (:in channels)
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
      (if-let [routed (secretary/dispatch!
                       (str "/" (more/decode-keyword target)))]
        (let [n (nav-entry/add-to-hierarchy nav [routed])
              id (first routed)]
          (may-abort id n #(do-hop (into [id] (get-in n [:hierarchy id :data]))
                                   (may-create-link n id))))
        nav))))
