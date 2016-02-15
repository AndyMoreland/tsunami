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

(defun tsunami--parse-symbols-response (response)
  (let* ((response (tsunami--get-response-body response))
         (symbol-locations (plist-get response :symbolLocations)))
    symbol-locations))

(defun tsunami--symbol-to-tuple (symbol)
  `(,(tsunami--name-of-symbol symbol) . ,symbol))

(defun tsunami--symbol-locations-to-candidates (symbol-locations)
  (mapcdr 'tsunami--symbol-to-tuple symbol-locations))

(defun tsunami--jump-to-matching-symbol (candidate)
  (let* ((location (tsunami--location-of-symbol candidate))
         (filename (plist-get location :filename))
         (pos (plist-get location :pos)))
    (find-file filename)
    (goto-char pos)))

(defun tsunami--relative-filename-to-module-name (relative-filename)
  (if (s-starts-with? "." relative-filename)
      relative-filename
    (concat "./" relative-filename)))

(defun tsunami--buffer-contains-regexp (regexp)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward-regexp regexp (point-max) t))))

(defun tsunami--module-imported-p (module-name)
  (if (or
       (tsunami--buffer-contains-regexp (concat "import .*? from '" module-name "'"))
       (tsunami--buffer-contains-regexp (concat "import .*? from \"" module-name "\"")))
      t
    nil))

(defun tsunami--import-module-name (module-name)
  (save-excursion
    (goto-char (point-min))
    (search-forward "import")
    (beginning-of-line)
    (insert-string (concat "import {} from \"" module-name "\";\n"))))

(defun tsunami--add-symbol-to-import (module-name symbol-name)
  (save-excursion
    (goto-char (point-min))
    (search-forward "import")
    (search-forward module-name)
    (beginning-of-line)
    (search-forward "{")
    (let ((empty-import-block-p (looking-at-p "}")))
      (insert-string (concat " " symbol-name))
      (if (not empty-import-block-p)
          (insert-string ", ")
        (insert-string " ")))))

(defun tsunami--import-symbol (module-name symbol-name)
  (let ((module-imported-p (tsunami--module-imported-p module-name)))
    (if (not module-imported-p)
        (tsunami--import-module-name module-name))
    (tsunami--add-symbol-to-import module-name symbol-name)))

(defun tsunami--import-symbol-location (candidate)
  (let* ((location (tsunami--location-of-symbol candidate))
         (symbol-name (tsunami--name-of-symbol candidate))
         (filename (plist-get location :filename))
         (relative-filename (file-relative-name filename (file-name-directory buffer-file-name)))
         (module-name (tsunami--relative-filename-to-module-name relative-filename)))
    (tsunami--import-symbol module-name symbol-name)))

(defun tsunami--symbols-helm-source ()
  "Define helm source for tsunami symbols."
  `((name . ,(concat "Searching for Symbols"))
    (candidates . ,(mapcar 'tsunami--symbol-to-tuple tsunami--matching-symbols))
    (volatile)
    (action . (("Jump To `RET'" . tsunami--jump-to-matching-symbol)
               ("Import" . tsunami--import-symbol-location)))))

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

(defun helm-tsunami-symbols ()
  "Search for symbols in project using helm."
  (interactive)
  (progn
    (tsunami--invalidate-symbols-cache)
    (helm :sources (tsunami--symbols-helm-source)
          :buffer "*helm-tsunami-symbols*")))

(provide 'tsunami)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; tsunami.el ends here
