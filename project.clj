(defproject longstorm/enlightened "0.2.3"
  :description "Fast interactive terminal (curses) UIs with cljs and nodejs."
  :url "https://github.com/longstorm/enlightened"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com/longstorm/enlightened.git"}
  :aliases {"cleantest" ["do" "clean," "cljsbuild" "once," "test,"]
            "autotest" ["do" "clean," "cljsbuild" "auto" "test"]}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]
                 [longstorm/claude "0.1.4"]]
  :cljsbuild
  {:builds [{:id "demo"
             :source-paths ["src/lib" "src/node" "demo"]
             :compiler {:target :nodejs
                        :output-to "resources/public/demo.js"
                        :optimizations :simple}}
            {:id "test"
             :source-paths ["src/lib" "test"]
             :notify-command ["phantomjs"
                              :cljs.test/runner "target/test.js"]
             :compiler {:libs [""]
                        :output-to "target/test.js"
                        :optimizations :whitespace
                        :pretty-print true}}]
            
   :test-commands {"unit-tests" ["phantomjs" :runner
                                 "target/test.js"]}}
  :profiles
  {:dev {:dependencies [[com.cemerick/double-check "0.5.7-SNAPSHOT"]]
         :node-dependencies [[phantomjs "1.9.x"]
                             [blessed "0.0.29"]]
         :hooks [leiningen.cljsbuild]
         :plugins [[lein-cljsbuild "1.0.3"]
                   [lein-npm "0.4.0"]
                   [com.cemerick/clojurescript.test "0.3.0"]]}})
