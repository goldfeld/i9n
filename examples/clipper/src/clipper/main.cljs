(ns clipper.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [claude.process :as proc]
            [i9n.os.term :as term]
            [i9n.os.navigation :refer [navigation-view]]))

(defn nav []
  [["clippings" ["quit" #(proc/exit)]]])

(defn make-paster [chan mode]
  #(a/pipe (term/paste)
           (a/map> (fn [p] [:fix [nil mode p nil]])
                   chan)))

(defn -main [& input]
  (let [chan (a/chan)]
    (term/bind-global-keys
     ["C-c" "q"] #(proc/exit)
     "p" (make-paster chan :insert-after)
     "S-p" (make-paster chan :insert)
     "x" #(a/put! chan [:fix [nil :remove]])
     "y" #(term/copy "copied text"))
    (let [[cmd & args] input]
      (navigation-view (nav) {:chan chan}))))

(set! *main-cli-fn* -main)
