(ns enlightened.nav-entry-test
  (:require [enlightened.nav-entry :as n]
            [cemerick.cljs.test
             :include-macros true :refer [deftest is testing]]
            [cemerick.double-check.clojure-test :as tc
             :include-macros true :refer [defspec]]
            [cemerick.double-check.generators :as gen]
            [cemerick.double-check.properties :as prop :include-macros true]))

(deftest create-hierarchy-simple-test
  (is (= (n/create-hierarchy ["menu" ["a" :a "b" :b]]
                             [[:a "a" "some text"]
                              [:b "b" ["c" :c]]
                              [:c "c" "other text"]])
         {:root ["menu" ["a" :a "b" :b]]
          :a ["a" "some text"]
          :b ["b" ["c" :c]]
          :c ["c" "other text"]})))
