(require 'tide)

(defun tsunami--get-quickinfo-at-point (cb)
  (tide-send-command "quickinfo" `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset)) cb))

(defun tsunami-jump-to-definition ()
  (interactive)
  (tsunami--get-quickinfo-at-point (lambda (response)
                                     (when (tide-response-success-p response)
                                       (if (string-equal "alias" (tide-plist-get response :body :kind))
                                           (tide-jump-to-definition 1)
                                         (tide-jump-to-definition))))))

(provide 'tsunami-navigation)
