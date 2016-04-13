(defun tsunami--helm-projectile-build-dwim-source (candidates action)
  "Dynamically build a Helm source definition for Projectile files based on context with CANDIDATES, executing ACTION."
  (helm-build-in-buffer-source "Projectile files"
    :data candidates
    :fuzzy-match helm-projectile-fuzzy-match
    :coerce 'helm-projectile-coerce-file
    :action-transformer 'helm-find-files-action-transformer
    :keymap helm-projectile-find-file-map
    :help-message helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action action))

(defvar tsunami--my-helm-result nil)

(defun tsunami--helm-read-file-in-project (prompt)
  (helm :sources (tsunami--helm-projectile-build-dwim-source
                  (projectile-current-project-files)
                  (lambda (x)
                    (setq tsunami--my-helm-result x)))
        :prompt prompt))

(defun tsunami-refactor-move-symbol ()
  (interactive)
  (let ((destination-file (tsunami--helm-read-file-in-project "Select destination module:")))
    (message destination-file)))

;; Should interactively edit both occurrences -- use multiple cursors?
(defun tsunami-refactor-extract-local (new-name)
  (interactive "MNew Name")
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (kill-region start end)
        (insert new-name)
        ;; (iedit-add-region-as-occurrence start (+ start (length new-name)))
        (open-line-above)
        (yank)
        (indent-according-to-mode)
        (back-to-indentation)
        (insert (concat "const " new-name " = "))
        (back-to-indentation)
        (forward-word)
        (forward-char)
        ;; (iedit-add-region-as-occurrence (point) (+ (point) (length new-name)))
        (end-of-line)
        (insert ";")))))


(defun tsunami-organize-imports ()
  "Organize the current file's imports."
  (interactive)
  (let ((buf-to-organize (current-buffer)))
    (tsunami--command:organize-imports (buffer-file-name buf-to-organize)
                                       (lambda (arg)
                                         (if (equal "null" (plist-get arg :body))
                                             (revert-buffer t t)
                                           (message "Failed to organize imports."))))))

(provide 'tsunami-refactor)
