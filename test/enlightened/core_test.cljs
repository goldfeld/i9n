(ns enlightened.core-test
  (:require-macros [cemerick.cljs.test :refer [deftest testing is]])
  (:require [cemerick.cljs.test :as t]
            [enlightened.core :as en]))

(deftest walk-up
  (is (= (count "hey") 3)))
