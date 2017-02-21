(defun tsunami--padded-format (length)
  (concat "%-" (number-to-string length) "." (number-to-string length) "s"))

(defun tsunami--filename-relative-to-buffer (filename)
  (file-relative-name filename (file-name-directory buffer-file-name)))

(defun goto-line-offset (line offset)
  "LINE and OFFSET are both 1-indexed."
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char (1- offset)))

(defun get-line-offset-pos (line offset)
  (save-excursion
    (goto-line-offset line offset)
    (point)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun tsunami--rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(provide 'tsunami-util)
