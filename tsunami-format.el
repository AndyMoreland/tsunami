(require 'tsunami-code-edit)
(require 'tide)

(defun tsunami-format ()
  (interactive)
  (let ((response (tide-send-command-sync "format" `(:file ,buffer-file-name
                                                     :line 1
                                                     :offset 1
                                                     :endLine ,(count-lines 1 (point-max))
                                                     :endOffset ,(save-excursion
                                                                   (goto-char (point-max))
                                                                   (count-lines 1 (point)))))))
    (when (tide-response-success-p response)
      (tsunami--apply-code-edits (plist-get response :body)))))

(provide 'tsunami-format)
