(require 'tide)
(require 'eldoc)

;;; Eldoc: For some reason I can't figure out how to get eldoc to not flicker. Right now just using regular `message`.
;;; Ripped entirely from tide. Rewrote to be async instead of sync.
(defun tsunami-annotate-display-part (display-part &optional highlight)
  (let ((text (plist-get display-part :text))
        (face (pcase (plist-get display-part :kind)
                ("aliasName" 'font-lock-type-face)
                ("className" 'font-lock-type-face)
                ("enumName" 'font-lock-type-face)
                ("fieldName" nil)
                ("interfaceName" 'font-lock-type-face)
                ("keyword" 'font-lock-keyword-face)
                ("lineBreak" nil)
                ("numericLiteral" nil)
                ("stringLiteral" 'font-lock-string-face)
                ("localName" 'font-lock-variable-name-face)
                ("methodName" nil)
                ("moduleName" nil)
                ("operator" nil)
                ("parameterName" (and highlight 'eldoc-highlight-function-argument))
                ("propertyName" nil)
                ("punctuation" nil)
                ("space" nil)
                ("text" nil)
                ("typeParameterName" 'font-lock-variable-name-face)
                ("enumMemberName" 'font-lock-constant-face)
                ("functionName" 'font-lock-function-name-face)
                ("regularExpressionLiteral" 'font-lock-string-face))))
    (if face
        (propertize text 'face face)
      text)))

(defun tsunami-annotate-signature-parameter (parameter highlight)
  (tide-join
   (-map
    (lambda (part) (tsunami-annotate-display-part part highlight))
    (plist-get parameter :displayParts))))

(defun tsunami-annotate-signature (signature selected-arg-index)
  (let ((separator (tide-join (-map #'tsunami-annotate-display-part (plist-get signature :separatorDisplayParts)))))
    (tide-join
     (-concat
      (-map #'tsunami-annotate-display-part (plist-get signature :prefixDisplayParts))
      (list
       (mapconcat
        #'identity
        (-map-indexed
         (lambda (i parameter)
           (tsunami-annotate-signature-parameter parameter (eq i selected-arg-index)))
         (plist-get signature :parameters))
        separator))
      (-map #'tsunami-annotate-display-part (plist-get signature :suffixDisplayParts))))))

(defun tsunami-annotate-signatures (body)
  (let ((selected-index (plist-get body :selectedItemIndex))
        (selected-arg-index (plist-get body :argumentIndex)))
    (tsunami-annotate-signature
     (nth selected-index (plist-get body :items))
     selected-arg-index)))

(defvar tsunami--last-eldoc nil)

;; Andy wrote this
(defun tsunami-command:signatureHelp ()
  (tide-send-command "signatureHelp"
                     `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset))
                     (lambda (response)
                       (when (tide-response-success-p response)
                         (let ((the-message (tsunami-annotate-signatures (plist-get response :body))))
                           (setq tsunami--last-eldoc the-message)
                           (eldoc-message the-message))))))

(defun tsunami-eldoc-quickinfo-callback (response)
  (when (tide-response-success-p response)
    (let* ((response (tsunami--get-response-body response))
           (display-string (plist-get response :displayString))
           (documentation (plist-get response :documentation))
           (the-message (if (< 0 (length documentation))
                            (concat display-string "\n'" (propertize documentation 'face 'font-lock-comment-face) "'")
                          display-string)))
      (tsunami-eldoc-display-in-minibuffer the-message))))

;; Andy wrote this
(defun tsunami-command:quickinfo (callback)
  (tide-send-command "quickinfo" `(:file ,buffer-file-name
                                   :line ,(count-lines 1 (point))
                                   :offset ,(tide-current-offset))
                     callback))

(defun tsunami-get-function-info ()
  (interactive)
  (tsunami-command:signatureHelp #'tsunami-eldoc-signature-help-callback))

(defun tsunami-method-call-p ()
  (or (looking-at "[(,]")
      (and (not (looking-at "\\sw"))
           (looking-back "[(,]\n?\\s-*"))))

(defun tsunami-eldoc-function ()
  (prog2
    (when (not (member last-command '(next-error previous-error flycheck-next-error flycheck-previous-error tsunami-get-function-info)))
      (if (tsunami-method-call-p)
          (tsunami-command:signatureHelp #'tsunami-eldoc-signature-help-callback)
        (when (looking-at "\\s_\\|\\sw\\|\n")
          (tsunami-command:quickinfo #'tsunami-eldoc-quickinfo-callback))))
    tsunami--last-eldoc))


(provide 'tsunami-eldoc)
