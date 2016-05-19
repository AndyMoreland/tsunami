(require 'tide)

(defun tsunami--get-quickinfo-at-point (cb)
  (tide-send-command "quickinfo" `(:file ,buffer-file-name
                                   :line ,(count-lines 1 (point))
                                   :offset ,(tide-current-offset))
                     cb))

(defun tsunami--jump-to-definition-response (response)
  (tide-on-response-success response
    (let ((filespan (car (plist-get response :body))))
      (tide-jump-to-filespan filespan t))))

(defun tsunami--jump-to-definition-and-retry (response)
  (if (tide-response-success-p response)
      (tsunami--jump-to-definition-response response)
    (tide-command:definition 'tsunami--jump-to-definition-response)))

(defun tsunami-jump-to-definition (&optional arg)
  (interactive "P")
  (lexical-let ((arg arg))
    (tsunami--get-quickinfo-at-point (lambda (response)
                                       (if (or arg (string-equal "alias" (tide-plist-get response :body :kind)))
                                           (tide-command:type-definition 'tsunami--jump-to-definition-and-retry)
                                         (tide-command:definition 'tsunami--jump-to-definition-and-retry))))))

(provide 'tsunami-navigation)
