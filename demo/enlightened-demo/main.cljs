(ns enlightened-demo.main
  (:require [cljs.nodejs :as node]
            [enlightened.core :refer [get-screen]]
            [enlightened.widgets :as enlightened :refer [menu-view]]))

(defn exit
  ([] (exit 0))
  ([code] (.exit node/process code)))

(defn -main [& args]
  (doto (get-screen)
    (.key (clj->js ["escape" "C-c"]) #(exit)))
  (menu-view [["my-menu" ["do" :do "think" :think]]
              [:do "focus" ["hey" :hey]]]
             identity []))

(set! *main-cli-fn* -main)
