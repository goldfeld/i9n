(ns kitchen-sink.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [claude.process :as proc]
            [i9n.os.term :as term]
            [i9n.os.navigation :refer [navigation-view]]))

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
    (term/render-deferred)
    chan))

(def lorem
  (str "Lorem ipsum dolores siamet"
       (clojure.string/join (repeat 888 " lorem ipsum dolores siamet"))
       ". Lorem ipsum dolores siamet."))

(defn nav []
  [["inkstick" ["view text" :Text
                "view book" :Book
                "test" :test
                "quit" #(proc/exit)]]
   [:Text "Text" lorem]
   [:Book "Book" ["ch.1" lorem "ch.2" "Second part" "ch.3" "The End"]]
   [:test "focus" ["sadly asynchronous" sad-async
                   "also sad but hip" :hip
                   "sysiphus" #(navigation-view (nav))
                   "jit" (fn [] [[:new "Menu" ["a" :ho "d" :d]]
                                 [:d "d" ["a" :ho]]])]]
   [:hip "" sad-async]
   [:ho "go" ["hum" (constantly "sad")]]])

(defn -main [& input]
  (term/bind-global-keys ["C-c"] #(proc/exit))
  (let [[cmd & args] input]
    (navigation-view (nav))))

(set! *main-cli-fn* -main)
