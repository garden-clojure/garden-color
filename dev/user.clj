(ns user
  (:require
   [cemerick.piggieback :as piggieback]
   [cljs.repl :as repl]
   [cljs.repl.node :as node]
   [weasel.repl.websocket]))

(defn ws-repl []
  (piggieback/cljs-repl
   (weasel.repl.websocket/repl-env
    :ip "0.0.0.0" :port 9123)))

(defn node-repl []
  (piggieback/cljs-repl
   (node/repl-env)
   :output-dir "out"
   :optimizations :none
   :cache-analysis true                
   :source-map true))
