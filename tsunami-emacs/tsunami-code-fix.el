(require 'tsunami-protocol)

(-map (lambda (e) (flycheck-error-fix-data e)) flycheck-current-errors)

;; from gnu.org
(defun tsunami--find-overlays-specifying (prop)
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))


(defun tsunami--get-error-codes-at-point ()
  (-some->> (tsunami--find-overlays-specifying 'flycheck-error)
            (-map (lambda (o) (overlay-get o 'flycheck-error)))
            (-map (lambda (e) (flycheck-error-fix-data e)))))

(defun tsunami-get-code-fixes ()
  (interactive)
  (if-let ((response (tsunami--command:get-code-fixes (buffer-file-name) (point) (1+ (point)) (tsunami--get-error-codes-at-point)))
           (_ (tide-response-success-p response))
           (body (tsunami--get-response-body response))
           (first-fix (first body))
           (file-changes (plist-get first-fix :changes))
           (first-file-changes (first file-changes))
           (code-edits (plist-get first-file-changes :textChanges)))
      (progn
        (tsunami--apply-code-edits code-edits)
        (tide-format))
    (message "Unable to fix at point.")))

(provide 'tsunami-code-fix)
