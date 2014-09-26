(ns i9n.action
  (:require [cljs.core.async :as a]
            [i9n.ext :refer [custom-i9n-action]]))

(defmethod custom-i9n-action :hop
  [{:keys [action]} {{in :in} :channels}]
  (a/put! in [:hop action]))

(defmethod custom-i9n-action :pick-option
  [{:keys [action args]} {:keys [selected channels]}]
  (doto (:in channels)
    (a/put! [:state (:state-id args) (:pick args)])
    (a/put! [:handle-returned-action action args selected])))

(def n-fn-arg-paths
  {:state [:nav :state]
   :in [:channels :in]
   :nav [:nav] :put [:put]})

(defmethod custom-i9n-action :n-fn
  [{:keys [action requested-args]} {:keys [handle-returned-action] :as more}]
  (handle-returned-action
   (action (reduce (fn [m req-arg]
                     (assoc m req-arg
                            (get-in more (n-fn-arg-paths req-arg))))
                   {} requested-args))))
