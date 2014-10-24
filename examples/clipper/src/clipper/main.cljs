(ns clipper.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [claude.process :as proc]
            [i9n.node.os :as os]
            [i9n.core :as i9n]))

(defn nav []
  [["clippings" ["quit" #(proc/exit)]]])

(defn make-paster [chan mode]
  #(a/pipe (os/paste)
           (a/map> (fn [p] [:fix [nil mode p nil]])
                   chan)))

(defn -main [& input]
  (let [chan (a/chan)]
    (os/bind-global-keys
     ["C-c" "q"] #(proc/exit)
     "p" (make-paster chan :insert-after)
     "S-p" (make-paster chan :insert)
     "x" #(a/put! chan [:fix [nil :remove]])
     "y" #(os/copy "copied text"))
    (let [[cmd & args] input]
      (i9n/navigation-view (nav) {:chan chan}))))

(set! *main-cli-fn* -main)
