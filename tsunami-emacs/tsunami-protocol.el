(require 'tide)

(defun tsunami--command:fetch-all-symbols (prefix cb)
  "Fetch all symbols."
  (tide-send-command "SYMBOL_LOCATIONS" `(:prefix ,prefix) cb))

(defun tsunami--command:organize-imports (filename cb)
  (tide-send-command "ORGANIZE_IMPORTS" `(:filename ,filename) cb))

(defun tsunami--command:get-containing-expressions (filename line offset)
  (tide-send-command-sync "GET_CONTAINING_EXPRESSIONS" `(:file ,filename
                                                         :line ,line
                                                         :offset ,offset)))

(defun tsunami--command:get-containing-scopes (filename line offset)
  (tide-send-command-sync "GET_CONTAINING_SCOPES" `(:file ,filename
                                                    :line ,line
                                                    :offset ,offset)))

(defun tsunami--command:change-request (filename line offset end-line end-offset new-text)
  (tide-send-command "change" `(:file ,filename
                                :line ,line
                                :offset ,offset
                                :endLine ,end-line
                                :endOffset ,end-offset
                                :insertString ,new-text)))

(defun tsunami--command:save-to-request (filename tmpfile)
  (tide-send-command "saveto" `(:file ,filename
                                :tmpfile ,tmpfile)))

(provide 'tsunami-protocol)
