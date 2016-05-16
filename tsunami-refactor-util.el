(require 'rx)
(defvar tsunami--symbol-regexp (rx (or
                                    (syntax symbol)
                                    (syntax word))))

(defun tsunami--snap-region-to-symbol ()
  (save-excursion
    (if (region-active-p)
        (let ((start (region-beginning))
              (end (region-end)))
          ;; Handle beginning
          (goto-char start)
          (re-search-backward tsunami--symbol-regexp)))))
