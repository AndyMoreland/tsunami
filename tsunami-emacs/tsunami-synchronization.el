(require 'tsunami-protocol)

(defvar tsunami-before-change-beginning nil)
(defvar tsunami-before-change-end nil)
(defvar tsunami-change-syncing-mode nil)

(defun tsunami--get-column-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun tsunami-before-change-function (_beginning end)
  (setq tsunami-before-change-end-line (line-number-at-pos end))
  (setq tsunami-before-change-end-offset (tsunami--get-column-at-pos end)))

;; TODO these line numbers are incorrect for deletion. Need to use the tsunami-before-change-beginning variables.
(defun tsunami-after-change-function (beginning end old-length)
  (let ((new-text (buffer-substring beginning end)))
    (tsunami--command:change-request (buffer-file-name)
                                     (line-number-at-pos beginning)
                                     (1+ (tsunami--get-column-at-pos beginning))
                                     tsunami-before-change-end-line
                                     (1+ tsunami-before-change-end-offset)
                                     new-text)))


(defun tsunami-view-server-view-of-file ()
  (interactive)
  (let ((tmp-file (make-temp-file "tsunami-debug")))
    (tsunami--command:save-to-request (buffer-file-name)
                                      tmp-file)
    (find-file-other-window tmp-file)
    (message tmp-file)))

(defun tsunami-start-syncing ()
  (interactive)
  (setq-local tsunami-change-syncing-mode t)
  (add-hook 'before-change-functions 'tsunami-before-change-function nil t)
  (add-hook 'after-change-functions 'tsunami-after-change-function nil t))

(defun tsunami-stop-syncing ()
  (interactive)
  (setq-local tsunami-change-syncing-mode nil)
  (remove-hook 'before-change-functions 'tsunami-before-change-function t)
  (remove-hook 'after-change-functions 'tsunami-after-change-function t))

(provide 'tsunami-synchronization)
