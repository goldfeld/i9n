(ns enlightened-demo.main
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [clojure.string :as strng]
            [claude.process :as proc]
            [enlightened.core :as core]
            [enlightened.navigation :refer [navigation-view]]))

(defn sad-async []
  (let [chan (a/chan)]
    (a/put! chan "Hop!")
    (a/put! chan "haha")
    (a/put! chan :hey)
    (a/put! chan "hooooo")
    (a/put! chan :ho)
    chan))

(defn nav []
  [["inkstick" ["view text" :Text
                "test" :test
                "quit" #(proc/exit)]]
   [:Text "Text" (str "Lorem ipsum dolores siamet"
                      (strng/join (repeat 888 " lorem ipsum dolores siamet"))
                      ". Lorem ipsum dolores siamet.")]
   [:test "focus" ["sadly asynchronous" #(sad-async)
                   "sysiphus" #(navigation-view (nav) identity {})
                   "hey" :hey]]
   [:hey "guess" ["a" :a "b" :b "c" :c]]
   [:ho "go" ["hum" (constantly "sad")]]])

(defn -main [& input]
  (core/bind-global-keys ["C-c" "q"] #(proc/exit))
  (navigation-view (nav) identity []))

(set! *main-cli-fn* -main)
