(ns wayfinder.tools)

(def compactor-tool-definitions
  [{:type "function"
    :function
    {:name "summarize-item"
     :description "Replace an item's content with a shorter summary. Set remember=true to also file the original content to long-term memory before summarizing — use this for any item containing knowledge worth retaining indefinitely."
     :parameters
     {:type "object"
      :properties
      {:id {:type "integer"
            :description "The ID of the item to summarize"}
       :summary {:type "string"
                 :description "The shortened summary to replace the item's data"}
       :remember {:type "boolean"
                  :description "If true, file the item's original content to long-term memory before summarizing. Use for anything worth keeping indefinitely."}}
      :required ["id" "summary"]}}}

   {:type "function"
    :function
    {:name "forget-item"
     :description "Remove an item from context entirely. Items previously filed to long-term memory (remember=true on summarize) will not be re-filed when forgotten."
     :parameters
     {:type "object"
      :properties
      {:id {:type "integer"
            :description "The ID of the item to forget"}}
      :required ["id"]}}}
   {:type "function"
    :function
    {:name "file-to-memory"
     :description "File an item's content to long-term memory without summarizing it. Use this for items that contain important knowledge but are already concise enough to stay in context as-is."
     :parameters
     {:type "object"
      :properties
      {:id {:type "integer"
            :description "The ID of the item to file to long-term memory"}}
      :required ["id"]}}}])

(def scribe-tool-definitions
  [{:type "function"
    :function
    {:name "list-memories"
     :description "List all memory files with their summaries"
     :parameters
     {:type "object"
      :properties {}}}}

   {:type "function"
    :function
    {:name "read-memory"
     :description "Read the full contents of a memory file"
     :parameters
     {:type "object"
      :properties
      {:path {:type "string"
              :description "The filename of the memory to read"}}
      :required ["path"]}}}

   {:type "function"
    :function
    {:name "write-memory"
     :description "Write a new memory file or overwrite an existing one. The first line must be a one-line summary of the content."
     :parameters
     {:type "object"
      :properties
      {:filename {:type "string"
                  :description "Filename for the memory (e.g. 'facts/ssh-key.md')"}
       :content {:type "string"
                 :description "Full content. First line must be a one-line summary."}}
      :required ["filename" "content"]}}}

   {:type "function"
    :function
    {:name "delete-memory"
     :description "Permanently delete a memory file"
     :parameters
     {:type "object"
      :properties
      {:path {:type "string"
              :description "The filename of the memory to delete"}}
      :required ["path"]}}}])

(def tool-definitions
  [{:type "function"
    :function
    {:name "reason"
     :description "Record your internal reasoning. This is private — the user cannot see it. Use this to think through what to do before acting."
     :parameters
     {:type "object"
      :properties
      {:thought {:type "string"
                 :description "Your internal reasoning"}}
      :required ["thought"]}}}

   {:type "function"
    :function
    {:name "check-messages"
     :description "Check if there are any unread messages. Returns list of pending message IDs with previews."
     :parameters
     {:type "object"
      :properties {}}}}

   {:type "function"
    :function
    {:name "view-message"
     :description "View the full content of a pending message"
     :parameters
     {:type "object"
      :properties
      {:message-id {:type "integer"
                    :description "The ID of the notification referencing this message"}}
      :required ["message-id"]}}}

   {:type "function"
    :function
    {:name "send-message"
     :description "Send a message to the user"
     :parameters
     {:type "object"
      :properties
      {:content {:type "string"
                 :description "The message content to send"}}
      :required ["content"]}}}

   {:type "function"
    :function
    {:name "shell-command"
     :description "Execute a shell command and return its output"
     :parameters
     {:type "object"
      :properties
      {:command {:type "string"
                 :description "The shell command to execute"}}
      :required ["command"]}}}

   {:type "function"
    :function
    {:name "read-file"
     :description "Read the contents of a file"
     :parameters
     {:type "object"
      :properties
      {:path {:type "string"
              :description "Absolute path to the file"}}
      :required ["path"]}}}

   {:type "function"
    :function
    {:name "recall"
     :description "Search long-term memory for information relevant to a query"
     :parameters
     {:type "object"
      :properties
      {:query {:type "string"
               :description "What to search for in memory"}}
      :required ["query"]}}}

   {:type "function"
    :function
    {:name "wait"
     :description "Pause before your next turn. Specify seconds (min 5, max 300). Use short delays when busy, long delays when idle."
     :parameters
     {:type "object"
      :properties
      {:seconds {:type "integer"
                 :description "Seconds to wait (5-300)"}}
      :required ["seconds"]}}}])
