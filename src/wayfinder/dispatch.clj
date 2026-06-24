(ns wayfinder.dispatch
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import [java.io File]))

(def pending-messages (atom {}))

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defmulti execute-action :action-type)

(defmethod execute-action :check-messages [_]
  (if-let [msgs (seq @pending-messages)]
    {:content (->> msgs
                   (map (fn [[id msg]] (format "id %d: %s" id (trunc msg 80))))
                   (clojure.string/join "\n"))}
    {:content "No unread messages."}))

(defmethod execute-action :view-message [{:keys [message-id]}]
  (if-let [msg (get @pending-messages message-id)]
    (do (swap! pending-messages dissoc message-id)
        {:content msg})
    {:content (str "No pending message with id " message-id)}))

(defmethod execute-action :send-message [{:keys [content]}]
  {:content "Message sent"})

(defmethod execute-action :shell-command [{:keys [command]}]
  (let [result (sh (System/getenv "SHELL_PATH") "-c" command)]
    {:content (str (:out result)
              (when (seq (:err result))
                (str "\n--- stderr ---\n" (:err result)))
              (when (not= 0 (:exit result))
                (str "\n--- exit code " (:exit result) " ---")))}))

(defmethod execute-action :read-file [{:keys [path]}]
  (if (.exists (File. path))
    {:content (slurp path)}
    {:content (str "File not found: " path)}))

(defmethod execute-action :default [action]
  {:content (str "Unknown action: " (:action-type action))})
