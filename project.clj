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
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]
                 [longstorm/claude "0.1.4"]
                 [secretary "1.2.1"]
                 [flow "0.1.4"]]
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
                        :output-to "examples/kitchen_sink/kitchen_sink.js"}}]}
  :profiles
  {:dev {:node-dependencies [[blessed "0.0.29"]
                             [blessed-contrib "0.0.8"]
                             [copy-paste "0.3.0"]]
         :hooks [leiningen.cljsbuild]
         :plugins [[lein-cljsbuild "1.0.3"]
                   [lein-npm "0.4.0"]]}})
