(require 'helm)
(require 'tsunami-data)

(defvar tsunami--symbol-name-length 40)
(defvar tsunami--module-name-length 60)
(defvar tsunami--matching-symbols nil)

(defvar tsunami--helm-target-buffer nil)
(defvar tsunami--helm-rendered-symbols nil)
(defvar tsunami--helm-displayed-candidates nil)

(defun tsunami--helm-display-name-for-symbol (symbol)
  (let* ((name (tsunami--name-of-symbol symbol))
         (module-name (tsunami--get-module-name-for-import-for-symbol symbol)))
    (format
     (concat
      (tsunami--padded-format tsunami--symbol-name-length)
      " "
      (tsunami--padded-format tsunami--module-name-length))
     name module-name)))

(defun tsunami--symbol-to-helm-tuple (symbol)
  `(,(tsunami--helm-display-name-for-symbol symbol) . ,symbol))

(defun tsunami--jump-to-matching-symbol (candidate)
  (let* ((location (tsunami--location-of-symbol candidate))
         (filename (plist-get location :filename))
         (pos (+ 1 (plist-get (plist-get location :span) :start))))
    (find-file filename)
    (goto-char pos)))

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

(defun tsunami--candidate-match-function (candidate)
  (let ((symbol-name (first (s-split "\\s-" candidate t))))
    (helm-default-match-function symbol-name)))

;;; Memoize tsunami--helm-displayed-candidates vs tsunami--matching-symbols
(defun tsunami--get-helm-candidates ()
  (with-current-buffer (or tsunami--helm-target-buffer (current-buffer))
    (if (eq tsunami--helm-rendered-symbols (tsunami--get-matching-symbols))
        tsunami--helm-displayed-candidates
      (setq tsunami--helm-rendered-symbols (tsunami--get-matching-symbols)
            tsunami--helm-displayed-candidates (mapcar (lambda (candidate) (tsunami--symbol-to-helm-tuple candidate)) (tsunami--get-matching-symbols))))))

;; (defun tsunami--symbols-helm-source (actions)
;;   "Define helm source for tsunami symbols."
;;   (helm-build-sync-source "Tsunami Symbols Source"
;;     :candidates (mapcar 'tsunami--symbol-to-helm-tuple tsunami--matching-symbols)
;;     :volatile t
;;     :match 'tsunami--candidate-match-function
;;     :matchplugin nil
;;     :migemo 'nomultimatch
;;     ;; :filtered-candidate-transformer 'helm-adaptive-sort
;;     :action actions))


(defun tsunami--symbols-helm-source (actions)
  "Define helm source for tsunami symbols."
  (helm-build-sync-source (concat "Tsunami Symbols Source: " (tide-project-name))
    :candidates #'tsunami--get-helm-candidates
    :volatile t
    :fuzzy-match t
    :action actions))

;; (defun tsunami--symbols-helm-source (actions)
;;   "Define helm source for tsunami symbols."
;;   (helm-build-sync-source "Tsunami Symbols Source"
;;     :candidates (tsunami--get-helm-candidates)
;;     :match (lambda (pattern) (message pattern))
;;     :matchplugin nil
;;     :migemo 'nomultimatch
;;     :fuzzy-match nil
;;     :action actions))

(defun tsunami--helm (actions &optional re-index input)
  "Search for symbols in project using helm."
  (let ((tsunami--helm-target-buffer (current-buffer)))
    (when re-index (tsunami--invalidate-symbols-cache))
    (helm :sources (tsunami--symbols-helm-source actions)
          :buffer "*helm-tsunami-symbols*"
          :input input)))

(provide 'tsunami-helm)
