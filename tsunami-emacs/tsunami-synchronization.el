(require 'tsunami-protocol)

(defvar tsunami-before-change-beginning nil)
(defvar tsunami-before-change-end nil)

(defun tsunami--get-column-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun tsunami-before-change-function (beginning end)
  ;; (message "hi")
  ;; (print (format "%d, %d" beginning end))
  (setq tsunami-before-change-beginning beginning)
  (setq tsunami-before-change-end end))

(defun tsunami-after-change-function (beginning end old-length)
  (let ((new-text (buffer-substring beginning end))
        (old-end (+ beginning old-length)))
    (tsunami--command:change-request buffer-file-name
                                     (1- (line-number-at-pos beginning))
                                     (1- )
                                     (1- (line-number-at-pos old-end))
                                     (1- )
                                     new-text)

(provide 'tsunami-synchronization)
