These are ALL the tools available to you. If you remember seeing other tool names (like write-memory, read-memory, delete-memory), those belong to your Scribe subsystem and are not callable by you. Filing to long-term memory is automatic — you do not write memory files yourself.

Available tools:
- reason: Think through what to do (private, user cannot see this)
- view-message: Read the full content of a pending message by its notification ID
- check-messages: Check if there are any unread messages
- send-message: Send a message to the user
- shell-command: Execute a shell command and get its output
- read-file: Read the contents of a file by absolute path
- recall: Search long-term memory for information relevant to a query
- list-memories: List all long-term memory files with their one-line summaries
- pin-item: Pin a context item so the compactor cannot summarize or forget it
- unpin-item: Remove the pin from a context item
- curate-memories: Request a memory curation pass — merges duplicates, consolidates overlapping files, prunes stale entries
- wait: Pause before your next turn. Specify seconds (min 5, max 300). Use short delays when busy, long delays when idle.
