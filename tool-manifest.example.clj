;; Example custom tool manifest for Wayfinder.
;;
;; Each file in the tools directory (default: ~/tools, i.e. /home/wayfinder/tools) is a
;; single top-level map. Copy this file there and rename it (the extension
;; must be .clj). Wayfinder watches the directory and hot-reloads on change.
;;
;; Every value in the map is data EXCEPT :exec. :exec is Clojure code,
;; evaluated once per reload, and must return a function of one map argument.
;; That map is the tool's :args with :default values applied first.
;;
;; Available in :exec (unqualified or aliased): clojure.core, str/ (clojure.string),
;; io/ (clojure.java.io), shell/ (clojure.java.shell), json/ (cheshire.core).

{:name "hello"
 :description "Greet a named person"
 :args {:who {:type :string
              :default "world"
              :description "Who to greet"}}
 :exec (fn [{:keys [who]}]
         (str "hello " who))}
