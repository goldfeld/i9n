(ns enlightened.nav-entry-test
  (:require [enlightened.nav-entry :as n]
            [cemerick.cljs.test
             :include-macros true :refer [deftest is testing]]
            [cemerick.double-check.clojure-test :as tc
             :include-macros true :refer [defspec]]
            [cemerick.double-check.generators :as gen]
            [cemerick.double-check.properties :as prop :include-macros true]))

(deftest create-nav-simple-test
  (is (= (n/create-nav ["menu" ["a" :a "b" :b]]
                       [[:a "a" "some text"]
                        [:b "b" ["c" :c]]
                        [:c "c" "other text"]])
         {:hierarchy
          {:root {:data ["menu" ["a" :a "b" :b]]}
           :a {:data ["a" "some text"]}
           :b {:data ["b" ["c" :c]]}
           :c {:data ["c" "other text"]}}})))

(deftest fill-body-test
  (is (= [1 2 3 nil nil nil] (n/fill-body [1 2 3] 2 4))))
