(defproject garden/garden-color "0.1.0-SNAPSHOT"
  :description "Utilities for working with color."
  :url "http://github.com/garden-clojure/garden-color"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.6.0"]]

  :source-paths
  ["src/clj" "src/cljs"]

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/test.check "0.6.1"]
     [org.clojure/clojurescript "0.0-2371"]
     [weasel "0.4.2"]
     [com.cemerick/piggieback "0.1.3"]]

    :source-paths
    ["src/clj" "src/cljs" "dev"]

    :plugins
    [[com.cemerick/austin "0.1.3"]
     [lein-cljsbuild "1.0.3"]]

    :repl-options
    {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

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
