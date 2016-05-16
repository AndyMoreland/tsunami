(defun tsunami--command:fetch-all-symbols (prefix cb)
  "Fetch all symbols."
  (tide-send-command "SYMBOL_LOCATIONS" `(:prefix ,prefix) cb))

(defun tsunami--command:organize-imports (filename cb)
  (tide-send-command "ORGANIZE_IMPORTS" `(:filename ,filename) cb))

(defun tsunami--command:get-containing-expressions (filename line offset)
  (tide-send-command-sync "GET_CONTAINING_EXPRESSIONS" `(:file ,filename
                                                    :line ,line
                                                    :offset ,offset)))

(provide 'tsunami-protocol)
