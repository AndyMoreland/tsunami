;; Ripped wholesale from smartparens. https://github.com/Fuco1/smartparens/blob/master/test/test-helper.el
(defmacro tsunami-test-with-temp-buffer (initial initform &rest forms)
  "Setup a new buffer, then run FORMS.
First, INITFORM are run in the newly created buffer.
Then `tsunami-mode' is turned on.  Then INITIAL is
inserted (it is expected to evaluate to string).  If INITIAL
contains | put point there as the initial position (the character
is then removed).  If it contains M, put mark there (the
character is then removed).
Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil))
       (with-temp-buffer
         (set-input-method nil)
         ,initform
         (tsunami-mode 1)
         (pop-to-buffer (current-buffer))
         (insert ,initial)
         (goto-char (point-min))
         (when (search-forward "M" nil t)
           (delete-char -1)
           (set-mark (point))
           (activate-mark))
         (goto-char (point-min))
         (when (search-forward "|" nil t)
           (delete-char -1))
         ,@forms))))

(provide 'tsunami-test-helper)
