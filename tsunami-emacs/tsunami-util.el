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

(provide 'tsunami-util)
