(ns enlightened-demo.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [clojure.string :as strng]
            [claude.process :as proc]
            [enlightened.core :as core]
            [enlightened.navigation :refer [navigation-view]]))

(defn sad-async []
  (let [chan (a/chan)]
    (a/put! chan :hop)
    (a/put! chan "Hop!")
    (a/put! chan "haha")
    (a/put! chan :hey)
    (a/put! chan "hooooo")
    (a/put! chan :ho)
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
                                 [:d "d" ["a" :ho]]])
                   "hey" :hey]]
   [:hey "guess" ["a" :a "b" :b "c" :c]]
   [:ho "go" ["hum" (constantly "sad")]]])

(defn -main [& input]
  (core/bind-global-keys ["C-c"] #(proc/exit))
  (navigation-view (nav)))

(set! *main-cli-fn* -main)
