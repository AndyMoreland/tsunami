(require 's)
(require 'tsunami-util)

(defun tsunami--get-buffer-text-for-range (range)
  (let ((start (1+ (plist-get range :start)))
        (end (1+ (plist-get range :end))))
    (buffer-substring start end)))

(defun tsunami--get-buffer-text-for-file-range (file-range)
  (with-current-buffer (find-file-noselect (plist-get file-range :file))
    (tsunami--get-buffer-text-for-range file-range)))

(defun tsunami--get-plain-text-for-range (range)
  (let ((text (tsunami--get-buffer-text-for-range range)))
    (progn
      (set-text-properties 0 (length text) nil text)
      text)))

(defun tsunami--get-plain-text-for-file-range (range)
  (let ((text (tsunami--get-buffer-text-for-file-range range)))
    (progn
      (set-text-properties 0 (length text) nil text)
      text)))

(defun tsunami--get-folded-plain-text-for-range (range)
  (let* ((raw-text (tsunami--get-plain-text-for-range range))
         (lines (s-split "\n" raw-text)))
    (if (< 1 (length lines))
        (concat (first lines) " ... " (s-trim-left (car (last lines))))
      (first lines))))

(defun tsunami--get-range-for-span (span)
  (-let [(&plist :start (&plist :line start-line :offset start-offset)
                 :end (&plist :line end-line :offset end-offset))
         span]
    `(:start ,(get-line-offset-pos start-line start-offset)
      :end ,(get-line-offset-pos end-line end-offset))))

(defun tsunami--get-file-range-for-file-span (file-span)
  (-let [(&plist :start (&plist :line start-line :offset start-offset)
                 :end (&plist :line end-line :offset end-offset)
                 :file file)
         file-span]
    `(:start ,(get-line-offset-pos start-line start-offset)
      :end ,(get-line-offset-pos end-line end-offset)
      :file ,file)))

(provide 'tsunami-data-util)
