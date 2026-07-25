(ns wayfinder.tools)

(def compactor-tool-definitions
  [{:type "function"
    :function
    {:name "summarize-item"
     :description "Merge multiple items into one concise summary. The summary replaces the newest item in the batch; all others are forgotten. Set remember=true to file the original content of all items to long-term memory before summarizing."
     :parameters
     {:type "object"
      :properties
      {:ids {:type "array"
             :items {:type "integer"}
             :description "IDs of items to merge and summarize"}
       :summary {:type "string"
                 :description "Consolidated summary replacing all listed items"}
       :remember {:type "boolean"
                  :description "If true, file original content of all items to long-term memory before summarizing. Use for knowledge worth keeping indefinitely."}}
      :required ["ids" "summary"]}}}

   {:type "function"
    :function
    {:name "forget-item"
     :description "Remove items from context entirely. Items previously filed to long-term memory (remember=true on summarize) will not be re-filed when forgotten."
     :parameters
     {:type "object"
      :properties
      {:ids {:type "array"
             :items {:type "integer"}
             :description "IDs of items to forget"}}
      :required ["ids"]}}}

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
    {:name "send-message"
     :description "Send a message to the user you live with. Delivery is asynchronous — they read it when they are available, not instantly. The user keeps and sees EVERY message you have already sent: never restate, rephrase or resend earlier content, even with new details added. Send only when you have something genuinely new to say; learning more about a topic you already reported is not a reason to report it again. Silence is always acceptable."
     :parameters
     {:type "object"
      :properties
      {:content {:type "string"
                 :description "The message content to send"}}
      :required ["content"]}}}

   {:type "function"
    :function
    {:name "shell-command"
     :description "Execute a shell command and get its output"
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
    {:name "remember"
     :description "Write a note directly to your long-term memory. Use this whenever you learn something worth keeping: facts about the user, decisions, system knowledge, ongoing projects. The write is immediate and guaranteed — no intermediary."
     :parameters
     {:type "object"
      :properties
      {:filename {:type "string"
                  :description "Topic path for the memory file, e.g. 'facts/user-name.md' or 'projects/garden.md'"}
       :content {:type "string"
                 :description "Full memory content. First line must be a one-line summary."}}
      :required ["filename" "content"]}}}

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
    {:name "list-memories"
     :description "List all long-term memory files with their one-line summaries. Use this to see what knowledge you have stored, then use recall or read-file to access specific content."
     :parameters
     {:type "object"
      :properties {}}}}

   {:type "function"
    :function
    {:name "curate-memories"
     :description "Request a memory curation pass. The scribe will review all stored memories, merge duplicates, consolidate overlapping files, and prune stale or low-quality entries."
     :parameters
     {:type "object"
      :properties {}}}}

   {:type "function"
    :function
    {:name "pin-item"
     :description "Pin a context item so the compactor cannot summarize or forget it. Use this for items that must stay in context verbatim — critical configs, active instructions, recent decisions you need to reference exactly."
     :parameters
     {:type "object"
      :properties
      {:id {:type "integer"
            :description "The context item ID to pin"}}
      :required ["id"]}}}

   {:type "function"
    :function
    {:name "unpin-item"
     :description "Remove the pin from a context item, allowing the compactor to summarize or forget it again."
     :parameters
     {:type "object"
      :properties
      {:id {:type "integer"
            :description "The context item ID to unpin"}}
      :required ["id"]}}}

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
