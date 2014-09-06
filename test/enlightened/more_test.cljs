(ns enlightened.more-test
  (:require [enlightened.more :as more]
            [cemerick.cljs.test
             :include-macros true :refer [deftest is testing]]
            [cemerick.double-check.clojure-test :as tc
             :include-macros true :refer [defspec]]
            [cemerick.double-check.generators :as gen]
            [cemerick.double-check.properties :as prop :include-macros true]))

(deftest index-of-test
  (is (= 4 (more/index-of "hey you" "you")))
  (is (= 2 (more/index-of "hey you" "your" "a" "b" "y" "o"))))

(deftest splice-test
  (is (= [1 2 3 4 5] (more/splice [1 2 5] 2 0 3 4)))
  (is (= [1 5] (more/splice [1 2 3 4 5] 1 3)))
  (is (= [1 2 :a :b 5] (more/splice [1 2 3 4 5] 2 2 :a :b))))
