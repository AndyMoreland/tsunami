;; From http://emacs.stackexchange.com/questions/7558/collapsing-undo-history?rq=1
(defun undo-collapse-begin (marker)
  "Mark the beginning of a collapsible undo block.
This must be followed with a call to undo-collapse-end with a marker
eq to this one."
  (push marker buffer-undo-list))

(defun undo-collapse-end (marker)
  "Collapse undo history until a matching marker."
  (cond
    ((eq (car buffer-undo-list) marker)
     (setq buffer-undo-list (cdr buffer-undo-list)))
    (t
     (let ((l buffer-undo-list))
       (while (not (eq (cadr l) marker))
         (cond
           ((null (cdr l))
            (error "undo-collapse-end with no matching marker"))
           ((eq (cadr l) nil)
            (setf (cdr l) (cddr l)))
           (t (setq l (cdr l)))))
       ;; remove the marker
       (setf (cdr l) (cddr l))))))

 (defmacro with-undo-collapse (&rest body)
  "Execute body, then collapse any resulting undo boundaries."
  (declare (indent 0))
  (let ((marker (list 'apply 'identity nil)) ; build a fresh list
        (buffer-var (make-symbol "buffer")))
    `(let ((,buffer-var (current-buffer)))
       (unwind-protect
            (progn
              (undo-collapse-begin ',marker)
              ,@body)
         (with-current-buffer ,buffer-var
           (undo-collapse-end ',marker))))))

(defmacro with-atomic-undo (&rest body)
  "Execute body in an atomic-change-group, and attempt to modify undo history such that
if undo is executed after the body completes, the point will return to its original position."
  `(with-undo-collapse
     (undo-boundary)
     (push (point) buffer-undo-list)
     (atomic-change-group
       ,@body)))

(defun to-whitespace-insensitive-pattern (str)
  (replace-regexp-in-string "\\s-" (concat (regexp-quote "\\s-") "+") str))

(provide 'tsunami-refactor-util)
