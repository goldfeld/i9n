(ns i9n.nav-entry-test
  (:require-macros [cemerick.cljs.test :refer [deftest is testing]])
  (:require [i9n.nav-entry :as n]))

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
