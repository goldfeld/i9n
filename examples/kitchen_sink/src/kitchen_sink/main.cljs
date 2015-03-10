(ns kitchen-sink.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [claude.process :as proc]
            [i9n.core :as i9n :include-macros true]
            [i9n.node.os :as os]
            [i9n.ext.log]))

(defn sad-async []
  (let [chan (a/chan)]
    (doto chan
      (a/put! [:fix [:hip :title "Hop!"]])
      (a/put! [:fix [:hip 0 "to non-async"]])
      (a/put! [:fix [:hip 1 :ho]])
      (a/put! [:add [:here "Here" ["text" :ho "book" :Book]]])
      (a/put! [:fix [:hip 2 "hooooo"]])
      (a/put! [:fix [:hip 3 :here]])
      (a/put! [:fix [:here 0 "ho"]])
      (a/put! [:next :hip])
      (a/close!))
    (os/render-deferred)
    chan))

(def lorem
  (str "Lorem ipsum dolores siamet"
       (clojure.string/join (repeat 888 " lorem ipsum dolores siamet"))
       ". Lorem ipsum dolores siamet."))

(defn nav [chan]
  [["kitchen sink" ["view text" :Text
                    "view book" :Book
                    "edit items" :edit
                    "log hey" #(cljs.core.async/put! chan [:log "heeee"])
                    "test" :test
                    #_"n-fn" #_(n [nav] (js/setTimeout #(.log js/console (clj->js nav)) 100)
                                  :Text)
                    "quit" #(proc/exit)]]
   [:Text "Text" lorem]
   [:Book "Book" ["ch.1" lorem "ch.2" "Second part" "ch.3" "The End"]]
   (i9n/editable :edit "edit these items"
                 (vec (interleave ["one" "two" "three" "four" "five" "six"]
                                  (repeat nil))))
   [:test "focus" ["sadly asynchronous" sad-async
                   "also sad but hip" :hip
                   "sysiphus" #(i9n/navigation-view (nav))
                   "jit" (fn [] [[:new "Menu" ["a" :ho "d" :d]]
                                 [:d "d" ["a" :ho]]])]]
   [:hip "" sad-async]
   [:ho "go" ["hum" (constantly "sad")]]])

(defn -main [& input]
  (os/bind-global-keys ["C-c"] #(proc/exit))
  (let [chan (a/chan)
        [cmd & args] input]
    (i9n/navigation-view (nav chan)
                         {:chan chan
                          :keybinds {"o"
                                     (fn [] (js/setTimeout #(.log js/console "main#57 " (clj->js @i9n.ext/operations)) 100))}})))

(set! *main-cli-fn* -main)
