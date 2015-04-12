(defproject i9n "0.2.13"
  :description "Fast declarative terminal (curses) UIs with cljs and nodejs."
  :url "https://github.com/goldfeld/i9n"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com/goldfeld/i9n.git"}
  :aliases {"cleantest" ["do" "clean," "cljsbuild" "once," "test,"]
            "autotest" ["do" "clean," "cljsbuild" "auto" "test"]}
  :source-paths ["src/lib" "src/os" "src/plugins"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3165"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [longstorm/claude "0.1.10"]
                 [secretary "1.2.3"]
                 [flow "0.1.6"]]
  :cljsbuild
  {:builds [{:id "example-clipper"
             :source-paths ["src/lib" "src/os" "src/plugins"
                            "examples/clipper/src"]
             :compiler {:target :nodejs :optimizations :simple
                        :output-to "examples/clipper/clipper.js"}}
            {:id "example-kitchen-sink"
             :source-paths ["src/lib" "src/os" "src/plugins"
                            "examples/kitchen_sink/src"]
             :compiler {:target :nodejs :optimizations :simple
                        :output-to "examples/kitchen_sink/kitchen_sink.js"}}
            {:id "test"
             :source-paths ["src/lib" "test"]
             :compiler {:output-to "target/test.js"
                        :optimizations :simple
                        :pretty-print true}}]
   :test-commands {"gen-tests" ["node" :node-runner
                                "target/test.js"]}}
  :profiles
  {:dev {:dependencies [[com.cemerick/double-check "0.6.1"]
                        [com.cemerick/piggieback "0.2.0"]
                        [org.clojure/tools.nrepl "0.2.10"]]
         :node-dependencies [[blessed "0.0.29"]
                             [blessed-contrib "0.0.8"]
                             [copy-paste "0.3.0"]]
         :plugins [[lein-cljsbuild "1.0.5"]
                   [lein-npm "0.5.0"]
                   [com.cemerick/clojurescript.test "0.3.3"]]
         :repl-options {:nrepl-middleware
                        [cemerick.piggieback/wrap-cljs-repl]}}})
