(defun tsunami--helm-display-name-for-symbol (symbol)
  (let* ((name (tsunami--name-of-symbol symbol))
         (module-name (tsunami--get-module-name-for-import-for-symbol symbol))
         (symbol-type (tsunami--type-of-symbol symbol)))
    (format
     (concat (tsunami--padded-format tsunami--symbol-name-length)
             " "
             (tsunami--padded-format tsunami--module-name-length)
             " %s")
     name module-name symbol-type)))

(defun tsunami--symbol-to-helm-tuple (symbol)
  `(,(tsunami--helm-display-name-for-symbol symbol) . ,symbol))

(defun tsunami--jump-to-matching-symbol (candidate)
  (let* ((location (tsunami--location-of-symbol candidate))
         (filename (plist-get location :filename))
         (pos (+ 1 (plist-get location :pos))))
    (find-file filename)
    (goto-char pos)))

(defun tsunami--json-is-falsy (value)
  (or
   (not value)
   (eq :json-false value)
   (eq :json-null value)))

(defun tsunami--complete-with-candidate (candidate)
  (let* ((symbol-name (tsunami--name-of-symbol candidate))
         (bounds (bounds-of-thing-at-point 'symbol))
         (start-of-thing-at-point (car bounds))
         (end-of-thing-at-point (cdr bounds)))
    (delete-region start-of-thing-at-point end-of-thing-at-point)
    (insert symbol-name)))

(defun tsunami--default-helm-actions ()
  '(("Jump To `RET'" . tsunami--jump-to-matching-symbol)
    ("Import" . tsunami--import-symbol-location)))

(defun tsunami--symbols-helm-source (actions)
  "Define helm source for tsunami symbols."
  (helm-build-sync-source
      "Tsunami Symbols Source"
    :candidates (mapcar 'tsunami--symbol-to-helm-tuple tsunami--matching-symbols)
    :volatile t
    :fuzzy-match t
    :filtered-candidate-transformer 'helm-adaptive-sort
    :action actions))

(defun tsunami--helm (actions &optional input)
  "Search for symbols in project using helm."
  (interactive)
  (progn
    (tsunami--invalidate-symbols-cache)
    (helm :sources (tsunami--symbols-helm-source actions)
          :buffer "*helm-tsunami-symbols*"
          :input input)))



(provide 'tsunami-helm)
