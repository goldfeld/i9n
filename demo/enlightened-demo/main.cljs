(ns enlightened-demo.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [clojure.string :as strng]
            [claude.process :as proc]
            [enlightened.core :as core]
            [enlightened.navigation :refer [navigation-view]]))

(defn sad-async []
  (let [chan (a/chan)]
    (doto chan
      (a/put! [:add [:hip "aha" ["ha" (fn [] nil)]]])
      (a/put! [:fix [:hip :title "Hop!"]])
      (a/put! [:fix [:hip 0 "to non-async"]])
      (a/put! [:fix [:hip 1 :ho]])
      (a/put! [:add [:here "Here" ["text" :ho "book" :Book]]])
      (a/put! [:fix [:hip 2 "hooooo"]])
      (a/put! [:fix [:hip 3 :here]])
      (a/put! [:fix [:here 0 "ho"]])
      (a/put! [:next :hip])
      (a/close!))
    (core/render-deferred)
    chan))

(def lorem
  (str "Lorem ipsum dolores siamet"
       (strng/join (repeat 888 " lorem ipsum dolores siamet"))
       ". Lorem ipsum dolores siamet."))

(defn nav []
  [["inkstick" ["view text" :Text
                "view book" :Book
                "test" :test
                "quit" #(proc/exit)]]
   [:Text "Text" lorem]
   [:Book "Book" ["ch.1" lorem "ch.2" "Second part" "ch.3" "The End"]]
   [:test "focus" ["sadly asynchronous" #(sad-async)
                   "sysiphus" #(navigation-view (nav))
                   "jit" (fn [] [[:new "Menu" ["a" :ho "d" :d]]
                                 [:d "d" ["a" :ho]]])]]
   [:ho "go" ["hum" (constantly "sad")]]])

(defn -main [& input]
  (core/bind-global-keys ["C-c"] #(proc/exit))
  (let [[cmd & args] input]
    (navigation-view (nav))))

(set! *main-cli-fn* -main)
