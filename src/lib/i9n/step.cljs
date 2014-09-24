(ns i9n.step
  (:require [i9n.ext :refer [custom-i9n-step]]
            [secretary.core :as secretary]))

(def *route-dispatch!* (atom identity))
(defn route-dispatch! [route] (@*route-dispatch!* route))
(defn set-route-dispatch! [fn] (reset! *route-dispatch!* fn))

(defmethod custom-i9n-step :route
  [{:keys [route dispatch]} nav more]
  (secretary/add-route! (str "/" route) dispatch)
  nav)
