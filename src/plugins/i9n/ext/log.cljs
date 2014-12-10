(ns i9n.ext.log
  (:require [i9n.ext :refer [custom-i9n-op]]
            [flow.datetime :as dt]
            [claude.etc :refer [home]]
            [claude.fs :refer [append-file-and-forget]]))

(defmethod custom-i9n-op :log
  [[cmd & msg] nav {:keys [cfg]}]
  (let [now (dt/now)]
    (append-file-and-forget (str "[" (dt/date-display now)
                                 "-" (dt/time-display now) "] "
                                 (clojure.string/join " " msg) "\n")
                            (or (:logfile cfg) (str (home) "/.i9n-log"))))
  nav)
