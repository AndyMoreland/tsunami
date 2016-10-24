(require 'popup)
(require 'helm)
(require 's)
(require 'tsunami-code-edit)
(require 'tsunami-refactor-util)
(require 'deferred)

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

(defun tsunami--get-folded-plain-text-for-range (range)
  (let* ((raw-text (tsunami--get-plain-text-for-range range))
         (lines (s-split "\n" raw-text)))
    (if (< 1 (length lines))
        (concat (first lines) " ... " (s-trim-left (car (last lines))))
      (first lines))))

(defun tsunami--get-containing-expression-ranges ()
  (let* ((file (buffer-file-name (current-buffer)))
         (line (1- (line-number-at-pos (point))))
         (offset (1- (current-column)))
         (response (tsunami--command:get-containing-expressions file line offset)))
    (if (tide-response-success-p response)
        (tsunami--get-response-body response)
      (error (concat "Failure: " (plist-get response :message))))))

(defun tsunami--get-containing-scope-ranges ()
  (let* ((file (buffer-file-name (current-buffer)))
         (line (1- (line-number-at-pos (point))))
         (offset (1- (current-column)))
         (response (tsunami--command:get-containing-scopes file line offset)))
    (if (tide-response-success-p response)
        (tsunami--get-response-body response)
      (error (concat "Failure: " (plist-get response :message))))))

(defun tsunami--choose-containing-expression ()
  (let* ((ranges (tsunami--get-containing-expression-ranges))
         (with-expressions (-annotate 'tsunami--get-folded-plain-text-for-range ranges))
         (popup-items (reverse (--map (destructuring-bind (text . range) it
                                        (popup-make-item text :value range))
                                      with-expressions))))
    (when (< 0 (length ranges))
      (popup-menu* popup-items))))

(defun tsunami--choose-containing-scope ()
  (let* ((ranges (tsunami--get-containing-scope-ranges))
         (with-scopes (-annotate 'tsunami--get-folded-plain-text-for-range ranges))
         (popup-items (reverse (--map (destructuring-bind (text . range) it
                                        (popup-make-item text :value range))
                                      with-scopes))))
    (when (< 0 (length ranges))
      (popup-menu* popup-items))))

;; Should interactively edit both occurrences -- use multiple cursors?
(defun tsunami--execute-extract-local (start end scope-start scope-end)
  (let* ((new-name "newVariable")
         (expression-contents (buffer-substring start end))
         (expression-pattern (to-whitespace-insensitive-pattern expression-contents))
         (start-marker (save-excursion
                         (goto-char start)
                         (point-marker))))

    (with-atomic-undo
     ;; Replace all expression instances
     (save-excursion
       (goto-char scope-start)
       (while (re-search-forward expression-pattern scope-end t)
         (replace-match new-name)))
     ;; Insert new name
     (save-excursion
       (goto-char start-marker)
       (kill-forward-chars (length new-name))
       (insert new-name)
       (goto-char scope-start)
       (open-line-below)
       (progn ; Generate the expression definition.
         (insert (concat "const " new-name " = " expression-contents ";"))
         (indent-according-to-mode))))
    (forward-char (length new-name))
    (iedit-mode)))

(defun tsunami-refactor-extract-local ()
  (interactive)
  (let ((expression-range (tsunami--choose-containing-expression))
        (scope-range (tsunami--choose-containing-scope)))
    (if (and expression-range scope-range)
        (tsunami--execute-extract-local (1+ (plist-get expression-range :start))
                                        (1+ (plist-get expression-range :end))
                                        (1+ (plist-get scope-range :start))
                                        (1+ (plist-get scope-range :end)))
      (error "Point not in valid scope or expression"))))

(defun tsunami-refactor-organize-imports ()
  "Organize the current file's imports."
  (interactive)
  (let ((buf-to-organize (current-buffer)))
    (tsunami--command:organize-imports (buffer-file-name buf-to-organize)
                                       (lambda (response)
                                         (if (tide-response-success-p response)
                                             (let ((code-edits (plist-get response :body)))
                                               (tsunami--apply-code-edits code-edits)
                                               (message "Organized imports."))
                                           (message "Failed to organize imports."))))))

(defun tsunami-copy-interface-body ()
  (interactive)
  (let ((response (tsunami--command:sync-definition)))
    (print response)
    (save-excursion
      (with-current-buffer (current-buffer)
        (tsunami--jump-to-definition-response response)
        (search-forward "{")
        (backward-char)
        (mark-sexp)
        (copy-region-as-kill (point) (mark))))))

(defun tsunami-move-symbol (arg)
  (interactive "P")
  (let* ((from-filename (if arg
                            (tsunami--helm-read-file-in-project "From: ")
                          (buffer-file-name)))
         (to-filename (tsunami--helm-read-file-in-project "To: "))
         (symbol-name (read-from-minibuffer "SymbolName: " (thing-at-point 'symbol)))
         (response (tsunami--command:move-symbol from-filename to-filename symbol-name)))
    (if (tide-response-success-p response)
        (let ((code-edit-groups (plist-get response :body)))
          (with-atomic-undo
           (save-excursion
             (-each code-edit-groups
               (lambda (code-edit-group)
                 (let ((file (plist-get code-edit-group :file))
                       (code-edits (plist-get code-edit-group :edits)))
                   (with-current-buffer (find-file-noselect file)
                     (tsunami--apply-code-edits code-edits)
                     (basic-save-buffer))))))))
      (error "Failed to move symbol"))))

(provide 'tsunami-refactor)
