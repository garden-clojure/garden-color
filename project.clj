(defproject garden/garden-color "0.1.0-SNAPSHOT"
  :description "Utilities for working with color."
  :url "http://github.com/garden-clojure/garden-color"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.8.0"]
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
    [[lein-cljsbuild "1.0.3"]]

    :repl-options
    {:init-ns garden.color
     :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

  :aliases
  {"auto-build"
   ["do" ["cljsbuild" "clean"] ["cljsbuild" "auto" "dev"]]}

  :cljsbuild
  {:builds [{:id "dev"
             :source-paths ["src/clj" "src/cljs" "dev"]
             :compiler {:output-to "resources/public/js/garden-color.dev.js"
                        :output-dir "resources/public/js/out"
                        :source-map "resources/public/js/garden-color.dev.js.map"
                        :optimizations :none}}]}) 
