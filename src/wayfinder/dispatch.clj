(ns wayfinder.dispatch
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string])
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

;; Groups: 1 = optional quote around the tag, 2 = the tag, 3 = the rest of the
;; opener line (a redirect target lives there and must stay visible).
(def ^:private heredoc-re
  #"<<-?[ \t]*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1([^\n]*)\n[\s\S]*?\n[ \t]*\2")

(defn- strip-payloads
  "Blank out heredoc bodies and prose string literals so the scan sees what the
   command DOES, not what it carries. Only quoted strings containing whitespace
   are stripped: `cat \"/var/lib/wayfinder/x\"` is a quoted path, i.e. use, while
   a sentence about the path has spaces in it. Returns nil when stripping is
   ambiguous (an unterminated heredoc, unbalanced quotes) — the caller then
   scans the raw string, so the uncertain case still denies."
  [command]
  (let [no-heredocs (clojure.string/replace command heredoc-re "HEREDOC$3")]
    (when-not (re-find #"<<" no-heredocs)
      (let [unescaped (clojure.string/replace no-heredocs #"\\." "")]
        (when (and (even? (count (filter #(= \' %) unescaped)))
                   (even? (count (filter #(= \" %) unescaped))))
          (-> unescaped
              (clojure.string/replace #"'[^']*\s[^']*'" "''")
              (clojure.string/replace #"\"[^\"]*\s[^\"]*\"" "\"\"")))))))

(defn- restricted-command?
  "The restricted path is denied when the command USES it. Mentioning it inside
   a heredoc or a quoted string is data, not access: denying that made the
   restriction itself unrecordable — the agent could not write the note saying
   the path is off-limits, so after every compaction it rediscovered and
   re-probed it. Returns the matched path."
  [command]
  (let [scanned (or (strip-payloads command) command)]
    (some #(re-find (re-pattern (java.util.regex.Pattern/quote %)) scanned) restricted-paths)))

(defn- denial-message [path]
  (str "Access denied: this command references the restricted path " path
    ". The path itself is off-limits (intentional, permanent). If you were only writing ABOUT it, rephrase without the literal path."))

(defmulti execute-action :action-type)

(defmethod execute-action :shell-command [{:keys [command]}]
  (if-let [path (restricted-command? command)]
    {:content (denial-message path)}
    (let [result (sh (System/getenv "SHELL_PATH") "-c" command)]
      {:content (trunc (str (:out result)
                        (when (seq (:err result))
                          (str "\n--- stderr ---\n" (:err result)))
                        (when (not= 0 (:exit result))
                          (str "\n--- exit code " (:exit result) " ---")))
                 max-result-length)})))

(defmethod execute-action :read-file [{:keys [path]}]
  (if-let [restricted-path (restricted? path)]
    {:content (str "Access denied: " restricted-path " is restricted (intentional, permanent).")}
    (if (.exists (File. path))
      {:content (trunc (slurp path) max-result-length)}
      {:content (str "File not found: " path)})))

(defmethod execute-action :default [action]
  {:content (str "Unknown action: " (:action-type action))})
