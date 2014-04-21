(defproject longstorm/enlightened "0.1.0"
  :description "Fast interactive terminal (curses) UIs with cljs and nodejs."
  :url "https://github.com/longstorm/enlightened"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com/longstorm/enlightened.git"}
  :plugins [[lein-cljsbuild "1.0.0-alpha2"]
            [lein-npm "0.4.0"]
            [com.cemerick/clojurescript.test "0.2.2"]]
  :hooks [leiningen.cljsbuild]
  :test-paths ["test"]
  :aliases {"cleantest" ["do" "clean," "cljsbuild" "once," "test,"]}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2014"]]
  :node-dependencies [[blessed "0.0.29"]]
  :cljsbuild {:builds [{:source-paths ["src" "test"]
                        :compiler {:target :nodejs
                                   :output-to "target/cljs/enlightened_test.js"
                                   :optimizations :advanced}}]
              :test-commands {"unit-tests" ["node" :node-runner
                                            "target/cljs/enlightened_test.js"]}})
