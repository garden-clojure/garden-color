(defproject garden/garden-color "1.0.0-SNAPSHOT"
  :description "Utilities for working with color."
  :url "http://github.com/garden-clojure/garden-color"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.9.0-alpha13"]
   [org.clojure/clojurescript "1.9.293" :scope "provided"]]

  :source-paths
  ["src/clj" "src/cljs"]

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/test.check "0.9.0"]
     [weasel "0.7.0-SNAPSHOT"]
     [com.cemerick/piggieback "0.2.1"]
     [org.clojure/tools.nrepl "0.2.10"]]

    :source-paths
    ["src/clj" "src/cljs" "dev"]

    :plugins
    [[lein-cljsbuild "1.1.4"]
     [lein-doo "0.1.7"]]

    :repl-options
    {:init-ns garden.color
     :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

  :aliases
  {"auto-build"
   ["do"
    ["clean"]
    ["cljsbuild" "auto" "dev"]]

   "test-all"
   ["do"
    ["test"]
    ["test-cljs"]]

   "test-cljs"
   ["do"
    ["clean"]
    ["doo" "node" "nodejs-test" "once"]
    ["doo" "phantom" "phantomjs-test" "once"]]}

  :cljsbuild
  {:builds [{:id "dev"
             :source-paths ["src/clj" "src/cljs" "dev"]
             :compiler {:output-to "target/js/garden-color.dev.js"
                        :output-dir "target/js/out"
                        :source-map "target/js/garden-color.dev.js.map"
                        :optimizations :none}}
            {:id "nodejs-test"
             :source-paths ["src/cljs" "test"]
             :compiler {:output-to "target/js/garden-color-node-test.js"
                        :target :nodejs
                        :main garden.test-runner
                        :optimizations :none}}
            {:id "phantomjs-test"
             :source-paths ["src/cljs" "test"]
             :compiler {:output-to "target/js/garden-color-phantomjs-test.js"
                        :main garden.test-runner
                        :optimizations :none}}]}) 
