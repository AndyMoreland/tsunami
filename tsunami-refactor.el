(require 'popup)

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


;; (defun testing-popups ()
;;   (x-popup-menu `((50 50) ,(selected-frame))
;;                 `("Please choose expression"
;;                   ("Expressions: "
;;                    ("a" . "b")))))

(defvar tsunami--my-helm-result nil)

(defun tsunami--helm-read-file-in-project (prompt)
  (helm :sources (tsunami--helm-projectile-build-dwim-source
                  (projectile-current-project-files)
                  (lambda (x) (setq tsunami--my-helm-result x)))
        :prompt prompt))

(defun tsunami-refactor-move-symbol ()
  (interactive)
  (let ((destination-file (tsunami--helm-read-file-in-project "Select destination module:")))
    (message destination-file)))

(defun tsunami--get-buffer-text-for-range (range)
  (let ((start (1+ (plist-get range :start)))
        (end (1+ (plist-get range :end))))
    (buffer-substring start end)))

(defun tsunami--get-plain-text-for-range (range)
  (let ((text (tsunami--get-buffer-text-for-range range)))
    (progn
      (set-text-properties 0 (length text) nil text)
      text)))

(defun tsunami--get-containing-expression-ranges ()
  (when-let ((file (buffer-file-name (current-buffer)))
             (line (1- (line-number-at-pos (point))))
             (offset (1- (current-column))))
    (let ((response (tsunami--command:get-containing-expressions file line offset)))
      (if (tide-response-success-p response)
          (tsunami--get-response-body response)
        (error (concat "Failure: " (plist-get response :message)))))))

(defun tsunami--choose-containing-expression ()
  (let* ((ranges (tsunami--get-containing-expression-ranges))
         (with-expressions (-annotate 'tsunami--get-plain-text-for-range ranges))
         (single-line-expressions (-filter (lambda (tuple)
                                             (destructuring-bind (text . range)
                                                 tuple
                                               (not (s-contains-p "\n" text))))
                                           with-expressions))
         (popup-items (reverse (--map (destructuring-bind (text . range) it
                                        (popup-make-item text :value range))
                                      single-line-expressions))))
    (popup-menu* popup-items)))

;; Should interactively edit both occurrences -- use multiple cursors?
(defun tsunami--execute-extract-local (start end)
  (let ((new-name "newVariable"))
    (save-excursion
      (goto-char start)
      (kill-region start end)
      (insert new-name)
      ;; (iedit-add-region-as-occurrence start (+ start (length new-name)))
      (open-line-above)
      (yank)
      (indent-according-to-mode)
      (back-to-indentation)
      (insert (concat "const " new-name " = " ))
      (back-to-indentation)
      (forward-word)
      (forward-char)
      ;; (iedit-add-region-as-occurrence (point) (+ (point) (length new-name)))
      (end-of-line)
      (insert ";"))
    (forward-char (length new-name))
    (iedit-mode)))

(defun tsunami-refactor-extract-local ()
  (interactive)
  (let ((expression-range (tsunami--choose-containing-expression)))
    (tsunami--execute-extract-local (1+ (plist-get expression-range :start))
                                    (1+ (plist-get expression-range :end)))))

(defun tsunami-refactor-organize-imports ()
  "Organize the current file's imports."
  (interactive)
  (let ((buf-to-organize (current-buffer)))
    (tsunami--command:organize-imports (buffer-file-name buf-to-organize)
                                       (lambda (arg)
                                         (if (equal "null" (plist-get arg :body))
                                             (revert-buffer t t)
                                           (message "Failed to organize imports."))))))

(provide 'tsunami-refactor)
