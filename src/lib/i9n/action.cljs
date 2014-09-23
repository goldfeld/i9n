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
