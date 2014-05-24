(defproject longstorm/enlightened "0.1.2-SNAPSHOT"
  :description "Fast interactive terminal (curses) UIs with cljs and nodejs."
  :url "https://github.com/longstorm/enlightened"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com/longstorm/enlightened.git"}
  :aliases {"auto" ["do" "clean," "cljsbuild" "auto"]}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2014"]]
  :cljsbuild {:builds [{:id "demo"
                        :source-paths ["src" "demo"
                                       ".lein-git-deps/claude/src"]
                        :compiler {:target :nodejs
                                   :output-to "resources/public/demo.js"
                                   :optimizations :simple}}]}
  :profiles
  {:dev {:dependencies [[org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]
                        [org.bodil/redlobster "0.2.1"]]
         :hooks [leiningen.cljsbuild]
         :plugins [[lein-cljsbuild "1.0.0-alpha2"]
                   [lein-git-deps "0.0.1-SNAPSHOT"]
                   [lein-npm "0.4.0"]]
         :git-dependencies [["https://github.com/longstorm/claude.git"]]
         :node-dependencies [[blessed "0.0.29"]]}})
