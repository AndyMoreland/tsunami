(require 'tide)
(require 'dash)
(require 'tsunami-data-util)


(defun tsunami--get-quickinfo-at-point (cb)
  (tide-send-command "quickinfo" `(:file ,buffer-file-name
                                   :line ,(count-lines 1 (point))
                                   :offset ,(tide-current-offset))
                     cb))

(defun tsunami--sync-get-quickinfo-at-point ()
  (tide-send-command-sync "quickinfo" `(:file ,buffer-file-name
                                   :line ,(count-lines 1 (point))
                                   :offset ,(tide-current-offset))))

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
                                       (if arg
                                           (tide-command:type-definition 'tsunami--jump-to-definition-and-retry)
                                         (tide-command:definition 'tsunami--jump-to-definition-and-retry))))))

(defun tsunami-references ()
  (interactive)
  (let ((reference-window (tide-references)))
    (when reference-window
      (select-window reference-window))))

(defun tsunami--find-tide-reference-on-line ()
  (unless (get-text-property (point) 'tide-reference)
    (beginning-of-line)
    (goto-char (next-single-property-change (point) 'tide-reference))))

(defun tsunami-implementations ()
  (interactive)
  (-when-let* ((response (tsunami--command:get-implementation (buffer-file-name)
                                                              (tide-line-number-at-pos (point))
                                                              (tide-current-offset)))
               (_ (tide-response-success-p response))
               (spans (tsunami--get-response-body response))
               (items (-map (lambda (file-span)
                              (popup-make-item (plist-get file-span :file)
                                               :value file-span))
                            spans)))
    (-let* ((item (popup-menu* items))
            ((&plist :file file :start (&plist :line line :offset offset)) item))
      (find-file file)
      (goto-line-offset line offset))))

;; Jump to the reference on the current line before `goto`ing it.
(advice-add 'tide-goto-reference :before #'tsunami--find-tide-reference-on-line)

(provide 'tsunami-navigation)
