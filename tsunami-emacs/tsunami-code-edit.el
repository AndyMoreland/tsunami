;; code-edit := start: Location, end: Location, newText: string
;; location := line: number, offset: number

(require 'dash)

(defun goto-line-offset (line offset)
  "LINE and OFFSET are both 1-indexed."
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char (1- offset)))

(defun get-line-offset-pos (line offset)
  (save-excursion
    (goto-line-offset line offset)
    (point)))

(cl-defstruct edit-span begin end text)

(defun edit-span-width (edit-span)
  (- (edit-span-end edit-span) (edit-span-begin edit-span)))

(defun tsunami--sort-by-begin (edit-spans)
  (sort edit-spans (lambda (a b)
                     (< (edit-span-begin a)
                        (edit-span-begin b)))))

(defun tsunami--edit-span-from-code-edit (code-edit)
  (let* ((start (plist-get code-edit :start))
         (end (plist-get code-edit :end))
         (start-pos (get-line-offset-pos (plist-get start :line) (plist-get start :offset)))
         (end-pos (get-line-offset-pos (plist-get end :line) (plist-get end :offset))))
    (make-edit-span :begin start-pos :end end-pos :text (plist-get code-edit :newText))))

(defun tsunami--apply-edit-span (edit-span)
  (save-excursion
    (goto-char (edit-span-begin edit-span))
    (delete-region (edit-span-begin edit-span) (edit-span-end edit-span))
    (insert (edit-span-text edit-span))))

(defun tsunami--apply-edit-spans (edit-spans)
  (save-excursion
    (-each edit-spans 'tsunami--apply-edit-span)))

(defun tsunami--apply-code-edit (code-edit)
  (--> code-edit
       (tsunami--edit-span-from-code-edit it)
       (tsunami--apply-edit-span it)))

(defun tsunami--apply-code-edits (code-edits)
  (--> code-edits
       (mapcar 'tsunami--edit-span-from-code-edit it)
       (reverse it)
       (tsunami--apply-edit-spans it)))

(provide 'tsunami-code-edit)
