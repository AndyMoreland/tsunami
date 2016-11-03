(require 'tide)

(defun tsunami--command:fetch-all-symbols (prefix cb)
  "Fetch all symbols."
  (tide-send-command "SYMBOL_LOCATIONS" `(:prefix ,prefix) cb))

(defun tsunami--command:organize-imports (filename cb)
  (tide-send-command "ORGANIZE_IMPORTS" `(:filename ,filename) cb))

(defun tsunami--command:implement-interface (filename position cb)
  (tide-send-command "IMPLEMENT_INTERFACE" `(:filename ,filename :position ,position) cb))

(defun tsunami--command:get-containing-expressions (filename line offset)
  (tide-send-command-sync "GET_CONTAINING_EXPRESSIONS" `(:file ,filename
                                                         :line ,line
                                                         :offset ,offset)))

(defun tsunami--command:get-properties-of-symbol (filename line offset)
  (tide-send-command-sync "GET_PROPERTIES_OF_SYMBOL" `(:file ,filename
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

(defun tsunami--command:sync-definition ()
  (tide-send-command-sync
   "definition"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tsunami--command:move-symbol (from-filename to-filename symbol-name)
  (tide-send-command-sync
   "MOVE_SYMBOL"
   `(:fromFilename ,from-filename :toFilename ,to-filename :symbolName ,symbol-name)))


(provide 'tsunami-protocol)
