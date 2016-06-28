(require 'ert)
(require 'tsunami-code-edit)

(defvar tsunami--replace-code-edit '(:start (:line 1 :offset 1) :end (:line 1 :offset 5) :newText "1234"))
(defvar tsunami--delete-code-edit '(:start (:line 2 :offset 1) :end (:line 2 :offset 4) :newText ""))
(defvar tsunami--insert-code-edit '(:start (:line 3 :offset 1) :end (:line 3 :offset 1) :newText "xyz"))

(defvar test-dir "/Users/amoreland/tsunami/test/")

(defmacro with-fixture-file (filename &rest forms)
  `(with-temp-buffer
     (insert-file-contents ,filename)
     ,@forms))

(defmacro test-function-with-args (name fixture-file expected-result-file function &rest args)
  `(ert-deftest ,name ()
     (let* ((fixture-ab (concat test-dir ,fixture-file))
            (actual (with-fixture-file fixture-ab
                                       (,function ,@args)
                                       (buffer-string)))
            (expected-ab (concat test-dir ,expected-result-file))
            (expected (with-temp-buffer
                        (insert-file-contents expected-ab)
                        (buffer-string))))
       (should
        (equal actual expected)))))

(defmacro test-code-edit (name fixture-file code-edit expected-result-file)
  `(test-function-with-args ,name ,fixture-file ,expected-result-file tsunami--apply-code-edit ,code-edit))

(defmacro test-code-edits (name fixture-file code-edits expected-result-file)
  `(test-function-with-args ,name ,fixture-file ,expected-result-file tsunami--apply-code-edits ,code-edits))

(test-code-edit tsunami-test-replace-edit
                "fixtures/example1.txt"
                tsunami--replace-code-edit
                "fixtures/replace-result.txt")

(test-code-edit tsunami-test-insert-edit
                "fixtures/example1.txt"
                tsunami--insert-code-edit
                "fixtures/insert-result.txt")

(test-code-edit tsunami-test-delete-edit
                "fixtures/example1.txt"
                tsunami--delete-code-edit
                "fixtures/delete-result.txt")

(test-code-edits tsunami-test-replace-delete-insert
                 "fixtures/example1.txt"
                 `(,tsunami--replace-code-edit ,tsunami--delete-code-edit ,tsunami--insert-code-edit)
                 "fixtures/combined-result.txt")

(provide 'tsunami-test)
