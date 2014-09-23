(ns i9n.step
  (:require [secretary.core :as secretary]))

(def *route-dispatch!* (atom identity))
(defn route-dispatch! [route] (@*route-dispatch!* route))
(defn set-route-dispatch! [fn] (reset! *route-dispatch!* fn))

(defmulti i9n-step (fn [step-map nav more] (:i9n-step step-map)))
(defmethod i9n-step :default [step-map nav more] nav)

(defmethod i9n-step :route
  [{:keys [route dispatch]} nav more]
  (secretary/add-route! (str "/" route) dispatch)
  nav)
