;;; tsunami.el --- Tsunami: A bigger Tide mode. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Andy Moreland.

;; Author: Andy Morleand <andy@moreland.com>
;; URL: http://github.com/andymoreland/tsunami
;; Version: 0.0.1
;; Keywords: typescript

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar tsunami--matching-symbols nil)
(defvar helm-alive-p)

(require 's)

(defun tsunami--command:fetch-all-symbols (prefix cb)
  "Fetch all symbols."
  (tide-send-command "SYMBOL_LOCATIONS" `(:prefix ,prefix) cb))

(defun tsunami--command:organize-imports (filename cb)
  (tide-send-command "ORGANIZE_IMPORTS" `(:filename ,filename) cb))

(defun tsunami--invalidate-symbols-cache ()
  "Invalidate the symbols cache."
  (tsunami--command:fetch-all-symbols "foo"
                                      (lambda (symbols)
                                        (setq tsunami--matching-symbols (tsunami--parse-symbols-response symbols))
                                        (and (helm-alive-p) (helm-update)))))

(defun tsunami--get-response-body (response)
  (plist-get response :body))

(defun tsunami--name-of-symbol (symbol)
  (plist-get symbol :name))

(defun tsunami--location-of-symbol (symbol)
  (plist-get symbol :location))

(defun tsunami--is-from-external-module-p (symbol)
  (not (tsunami--json-is-falsy
        (plist-get (tsunami--location-of-symbol symbol) :isExternalModule))))

(defun tsunami--get-module-name-for-import-for-symbol (symbol)
  (let ((symbol-filename (plist-get (tsunami--location-of-symbol symbol) :filename)))
    (print symbol-filename)
    (if (tsunami--is-from-external-module-p symbol)
        symbol-filename
      (tsunami--relative-filename-to-module-name
       (tsunami--filename-relative-to-buffer symbol-filename)))))

(defun tsunami--parse-symbols-response (response)
  (let* ((response (tsunami--get-response-body response))
         (symbol-locations (plist-get response :symbolLocations)))
    symbol-locations))

(defvar tsunami--symbol-name-length 40)
(defvar tsunami--module-name-length 60)

(defun tsunami--helm-display-name-for-symbol (symbol)
  (let* ((name (s-truncate (- tsunami--symbol-name-length 2)
                           (tsunami--name-of-symbol symbol)))
         (name-padding (make-string
                        (- tsunami--symbol-name-length (length name))
                        ? ))
         (module-name (s-truncate (- tsunami--module-name-length 2)
                                  (tsunami--get-module-name-for-import-for-symbol symbol)))
         (module-name-padding (make-string
                               (- tsunami--module-name-length (length module-name))
                               ? ))
         (symbol-type (plist-get symbol :type)))
    (concat name name-padding module-name module-name-padding symbol-type)))

(defun tsunami--symbol-to-helm-tuple (symbol)
  `(,(tsunami--helm-display-name-for-symbol symbol) . ,symbol))

(defun tsunami--jump-to-matching-symbol (candidate)
  (let* ((location (tsunami--location-of-symbol candidate))
         (filename (plist-get location :filename))
         (pos (+ 1 (plist-get location :pos))))
    (find-file filename)
    (goto-char pos)))

(defun tsunami--relative-filename-to-module-name (relative-filename)
  (let ((module-without-ts (replace-regexp-in-string "\\.tsx?" "" relative-filename)))
    (if (s-starts-with? "." module-without-ts)
        module-without-ts
      (concat "./" module-without-ts))))

(defun tsunami--buffer-contains-regexp (regexp)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward-regexp regexp (point-max) t))))

(defun tsunami--module-imported-p (module-name)
  (let ((string-regexp (regexp-opt '("\"" "'"))))
    (if (tsunami--buffer-contains-regexp (concat "import .*? from " string-regexp module-name string-regexp))
        t
      nil)))

(defun tsunami--import-module-name (module-name)
  (save-excursion
    (goto-char (point-min))
    (search-forward "import" nil t)
    (beginning-of-line)
    (insert (concat "import {} from \"" module-name "\";\n"))))

(defun tsunami--goto-import-block-for-module (module-name)
  (goto-char (point-min))
    (search-forward "import")
    (search-forward-regexp (concat module-name (regexp-opt '("\"" "'")) ";"))
    (beginning-of-line)
    (search-forward "{"))

(defun tsunami--add-symbol-to-import (module-name symbol-name is-default-p)
  (save-excursion
    (tsunami--goto-import-block-for-module module-name)
    (let ((empty-import-block-p (looking-at-p "}")))
      (insert
       (if is-default-p
           (concat " default as " symbol-name)
         (concat " " symbol-name)))
      (if (not empty-import-block-p)
          (insert ",")
        (insert " ")))))

(defun tsunami--add-default-import-symbol (module-name symbol-name)
  (save-excursion
    (tsunami--goto-import-block-for-module module-name)
    (backward-char)
    (delete-char 2)
    (insert symbol-name)))

(defun tsunami--import-symbol (module-name symbol-name is-default-p)
  (let ((module-imported-p (tsunami--module-imported-p module-name)))
    (if (not module-imported-p)
        (tsunami--import-module-name module-name))
    (if (and is-default-p
             (not module-imported-p))
        (tsunami--add-default-import-symbol module-name symbol-name)
      (tsunami--add-symbol-to-import module-name symbol-name is-default-p))))

(defun tsunami--filename-relative-to-buffer (filename)
  (file-relative-name filename (file-name-directory buffer-file-name)))

;; TODO: unit test this, or extract into typescript logic.
(defun tsunami--symbol-imported-p (module-name symbol-name is-default-p)
  (let ((string-regexp (regexp-opt '("\"" "'")))
        (import-specifier-left-regexp
         (if is-default-p
             ""
           (concat ".*?" (regexp-opt '("{ " "{" ", ")))))
        (import-specifier-right-regexp
         (if is-default-p
             ""
           (concat (regexp-opt '(" }" "}" ", ")) ".*?"))))
    (or
     (tsunami--buffer-contains-regexp
      (concat "import "
              import-specifier-left-regexp
              symbol-name
              import-specifier-right-regexp
              " from "
              string-regexp module-name string-regexp))
     (and is-default-p
          (tsunami--buffer-contains-regexp
           (concat "import {.*?default as " symbol-name ".*?} from " string-regexp module-name string-regexp))))))

(defun tsunami--json-is-falsy (value)
  (or
   (not value)
   (eq :json-false value)
   (eq :json-null value)))

(defun tsunami--symbol-is-default-export-p (candidate)
  (not
   (tsunami--json-is-falsy (plist-get candidate :default))))

(defun tsunami--import-symbol-location (candidate)
  (let* ((symbol-name (tsunami--name-of-symbol candidate))
         (module-name (tsunami--get-module-name-for-import-for-symbol candidate))
         (is-default-p (tsunami--symbol-is-default-export-p candidate)))
    (if (not (tsunami--symbol-imported-p module-name symbol-name is-default-p))
        (tsunami--import-symbol module-name symbol-name is-default-p))))

(defun tsunami--complete-with-candidate (candidate)
  (let* ((symbol-name (tsunami--name-of-symbol candidate))
         (bounds (bounds-of-thing-at-point 'symbol))
         (start-of-thing-at-point (car bounds))
         (end-of-thing-at-point (cdr bounds)))
    (delete-region start-of-thing-at-point end-of-thing-at-point)
    (insert symbol-name)))

(defun tsunami--import-and-complete-symbol (candidate)
  (tsunami--import-symbol-location candidate)
  (tsunami--complete-with-candidate candidate))

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

(defun tsunami-organize-imports ()
  "Organize the current file's imports."
  (interactive)
  (let ((buf-to-organize (current-buffer)))
    (tsunami--command:organize-imports (buffer-file-name buf-to-organize)
                                       (lambda (arg)
                                         (if (equal "null" (plist-get arg :body))
                                             (revert-buffer t t)
                                           (message "Failed to organize imports."))))))

(defun tsunami-fetch-all-symbols ()
  (interactive)
  (tsunami--command:fetch-all-symbols "fish" (lambda (response) (print response))))

(defun tsunami--helm (actions &optional input)
  "Search for symbols in project using helm."
  (interactive)
  (progn
    (tsunami--invalidate-symbols-cache)
    (helm :sources (tsunami--symbols-helm-source actions)
          :buffer "*helm-tsunami-symbols*"
          :input input)))

(defun tsunami-import-symbol-at-point ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (tsunami--helm '(("Import `RET'" . tsunami--import-and-complete-symbol)) symbol)))

(defun helm-tsunami-symbols ()
  (interactive)
  (tsunami--helm (tsunami--default-helm-actions)))

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

(define-minor-mode tsunami-mode
  "Toggle tsunami-mode" ;; doc
  :init-value nil ;; init
  :lighter "tsunami" ;; lighter
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-<return>") 'tsunami-import-symbol-at-point)
            (define-key map (kbd "C-c C-i") 'helm-tsunami-symbols)
            map))

(provide 'tsunami)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; tsunami.el ends here
