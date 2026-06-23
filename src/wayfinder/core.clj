(ns wayfinder.core
  (:require [wayfinder.context :as context]
            [wayfinder.agent :as agent]
            [clojure.edn :as edn])
  (:gen-class))

(defn load-config []
  (let [path (or (System/getenv "WAYFINDER_CONFIG") "wayfinder.edn")]
    (edn/read-string (slurp path))))

(defn -main [& args]
  (let [cfg (load-config)]
    (agent/run cfg)))
