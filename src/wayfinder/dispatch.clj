(ns wayfinder.dispatch
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import [java.io File]))

(def ^:private max-result-length 10000)

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(def restricted-paths
  ["/var/lib/wayfinder"])

(defn- restricted? [s]
  (some #(and (.startsWith (str s) %) %) restricted-paths))

(defn- restricted-command? [command]
  (some #(re-find (re-pattern (java.util.regex.Pattern/quote %)) command) restricted-paths))

(defmulti execute-action :action-type)

(defmethod execute-action :shell-command [{:keys [command]}]
  (if (restricted-command? command)
    {:content "Access denied: that path is restricted."}
    (let [result (sh (System/getenv "SHELL_PATH") "-c" command)]
      {:content (trunc (str (:out result)
                        (when (seq (:err result))
                          (str "\n--- stderr ---\n" (:err result)))
                        (when (not= 0 (:exit result))
                          (str "\n--- exit code " (:exit result) " ---")))
                 max-result-length)})))

(defmethod execute-action :read-file [{:keys [path]}]
  (if (restricted? path)
    {:content "Access denied: that path is restricted."}
    (if (.exists (File. path))
      {:content (trunc (slurp path) max-result-length)}
      {:content (str "File not found: " path)})))

(defmethod execute-action :default [action]
  {:content (str "Unknown action: " (:action-type action))})
