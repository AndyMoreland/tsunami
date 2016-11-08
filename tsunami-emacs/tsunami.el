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
(require 'hydra)
(require 'tide)
(require 'tsunami-eldoc)
(require 'tsunami-protocol)
(require 'tsunami-data)
(require 'tsunami-util)
(require 'tsunami-refactor)
(require 'tsunami-refactor-util)
(require 'tsunami-navigation)
(require 'tsunami-helm)
(require 'tsunami-synchronization)

(defun tsunami--invalidate-symbols-cache ()
  "Invalidate the symbols cache."
  (tsunami--command:fetch-all-symbols "foo"
                                      (lambda (symbols)
                                        (setq tsunami--matching-symbols (tsunami--parse-symbols-response symbols))
                                        (and (helm-alive-p) (progn
                                                              (message "Updating helm")
                                                              (helm-update))))))

(defun tsunami--get-module-name-for-import-for-symbol (symbol)
  (let ((symbol-filename (plist-get (tsunami--location-of-symbol symbol) :filename)))
    (if (tsunami--is-from-external-module-p symbol)
        symbol-filename
      (tsunami--relative-filename-to-module-name
       (tsunami--filename-relative-to-buffer symbol-filename)))))

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
    (when (tsunami--buffer-contains-regexp (concat "import .*? from " string-regexp module-name string-regexp))
      t)))

(defun tsunami--exact-matching-symbols (symbol-name)
  (-filter (lambda (def)
             (equal (plist-get def :name) symbol-name))
           tsunami--matching-symbols))

(defun tsunami--module-imported-wildcard-p (module-name)
  "Returns nil or string alias for MODULE-NAME"
  (let* ((quote-regexp (regexp-opt '("\"" "'")))
         (regexp (concat "import \\* as \\(.*?\\) from " quote-regexp module-name quote-regexp ";")))
    (print regexp)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (when (search-forward-regexp regexp nil t)
          (substring-no-properties (match-string 1)))))))

(defun tsunami--import-module-name (module-name)
  (goto-char (point-min))
  (search-forward "import" nil t)
  (beginning-of-line)
  (insert (concat "import __ from \"" module-name "\";\n"))
  (search-backward "__")
  (delete-char 2))

(defun tsunami--import-module-name-and-insert-brackets (module-name)
  (save-excursion
    (tsunami--import-module-name module-name)
    (insert "{}")))

(defun tsunami--goto-import-block-for-module (module-name)
  (goto-char (point-min))
    (search-forward "import")
    (search-forward-regexp (concat module-name (regexp-opt '("\"" "'")) ";"))
    (beginning-of-line)
    (search-forward "{"))

(defun tsunami--add-symbol-to-import (module-name symbol-name is-default-p)
  (save-excursion
    (tsunami--goto-import-block-for-module module-name)
    (let* ((empty-import-block-p (looking-at-p "}"))
           ;; Space-brace := { module }
           (space-brace-import-style-p (or
                                        (looking-at-p " ")
                                        empty-import-block-p)))
      (insert
       (if is-default-p
           (concat " default as " symbol-name)
         (concat (if space-brace-import-style-p " " "") symbol-name)))
      (if (not empty-import-block-p)
          (insert (if space-brace-import-style-p "," ", "))
        (when space-brace-import-style-p
          (insert  " "))))))

(defun tsunami--add-default-import-symbol (module-name symbol-name)
  (save-excursion
    (tsunami--goto-import-block-for-module module-name)
    (backward-char)
    (delete-char 2)
    (insert symbol-name)))

(defun tsunami--import-symbol (module-name symbol-name is-default-p)
  (let ((module-imported-p (tsunami--module-imported-p module-name)))
    (if (not module-imported-p)
        (tsunami--import-module-name-and-insert-brackets module-name))
    (if (and is-default-p
             (not module-imported-p))
        (tsunami--add-default-import-symbol module-name symbol-name)
      (tsunami--add-symbol-to-import module-name symbol-name is-default-p))))


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

(defun tsunami--import-external-module (module-name)
  (unless (tsunami--module-imported-p module-name)
    (tsunami--import-module-name module-name)
    (insert (concat "* as " module-name))))

(defun tsunami--import-symbol-location (candidate)
  (let* ((symbol-type (tsunami--type-of-symbol candidate))
         (module-name (tsunami--get-module-name-for-import-for-symbol candidate))
         (symbol-name (tsunami--name-of-symbol candidate))
         (is-default-p (tsunami--symbol-is-default-export-p candidate)))
    (cond ((equal "EXTERNAL_MODULE" symbol-type) (tsunami--import-external-module module-name))
          (t (if (not (tsunami--symbol-imported-p module-name symbol-name is-default-p))
                 (tsunami--import-symbol module-name symbol-name is-default-p))))))

(defun tsunami--import-and-complete-symbol (candidate)
  (with-atomic-undo
    (tsunami--import-symbol-location candidate)
    (tsunami--complete-with-candidate candidate)))

(defun tsunami-import-symbol-at-point (&optional arg)
  (interactive "P")
  (let* ((symbol (thing-at-point 'symbol t))
         (exact-matches (tsunami--exact-matching-symbols symbol)))
    (if (= 1 (length exact-matches))
        (tsunami--import-and-complete-symbol (-first-item exact-matches))
      (tsunami--helm '(("Import `RET'" . tsunami--import-and-complete-symbol)) arg symbol))))

(defun helm-tsunami-symbols (&optional arg)
  (interactive "P")
  (tsunami--helm (tsunami--default-helm-actions) arg))

(defun tsunami--setup-variables ()
  (setq-local eldoc-documentation-function 'tsunami-eldoc-function))

(defhydra tsunami-refactor-hydra (:color blue :hint nil)
  "
_l_ extract local
_i_ organize imports
_r_ rename symbol
_m_ move symbol
_f_ reformat
  "
  ("i" tsunami-refactor-organize-imports)
  ("l" tsunami-refactor-extract-local)
  ("r" tide-rename-symbol)
  ("m" tsunami-move-symbol)
  ("f" tide-format))

(define-minor-mode tsunami-mode
  "Toggle tsunami-mode" ;; doc
  :init-value nil ;; init
  :lighter " tsu" ;; lighter
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-<return>") 'tsunami-import-symbol-at-point)
            (define-key map (kbd "C-c C-i") 'helm-tsunami-symbols)
            (define-key map (kbd "M-.") 'tsunami-jump-to-definition)
            (define-key map (kbd "C-c C-r") 'tsunami-refactor-hydra/body)
            (define-key map (kbd "M-q") 'tsunami-get-function-info)
            (define-key tide-mode-map (kbd "M-.") 'tsunami-jump-to-definition)
            map)
  :after-hook (tsunami--setup-variables))

(provide 'tsunami)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; tsunami.el ends here
