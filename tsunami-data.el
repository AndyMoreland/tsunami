(defun tsunami--get-response-body (response)
  (plist-get response :body))

(defun tsunami--name-of-symbol (symbol)
  (plist-get symbol :name))

(defun tsunami--location-of-symbol (symbol)
  (plist-get symbol :location))

(defun tsunami--type-of-symbol (symbol)
  (plist-get symbol :type))

(defun tsunami--is-from-external-module-p (symbol)
  (not (tsunami--json-is-falsy
        (plist-get (tsunami--location-of-symbol symbol) :isExternalModule))))

(defun tsunami--parse-symbols-response (response)
  (let* ((response (tsunami--get-response-body response))
         (symbol-locations (plist-get response :symbolLocations)))
    symbol-locations))

(defun tsunami--json-is-falsy (value)
  (or
   (not value)
   (eq :json-false value)
   (eq :json-null value)))

(defun tsunami--symbol-is-default-export-p (candidate)
  (not
   (tsunami--json-is-falsy (plist-get candidate :default))))

(provide 'tsunami-data)
