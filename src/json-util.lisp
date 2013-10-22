(in-package :cl-user)
(defpackage log4cl-fluentd.json
  (:use :cl)
  (:import-from :cl-json
		:decode-json-from-string
		:encode-json-plist-to-string
		:*identifier-name-to-key*
		:*json-identifier-name-to-lisp*
		:*beginning-of-object-handler*
		:*object-key-handler*
		:*object-value-handler*
		:*end-of-object-handler*)
  (:export :decode-json-to-hash
	   :json))
(in-package :log4cl-fluentd.json)

(let (current-hash current-key)
  (defun handle-begin-object ()
    (setf current-hash (make-hash-table :test 'equal)))
  (defun handle-key (key)
    (setf current-key key))
  (defun handle-value (value)
    (setf (gethash current-key current-hash) value))
  (defun handle-end-object ()
    current-hash))

(defun decode-json-to-hash (str)
  (let ((*beginning-of-object-handler* #'handle-begin-object)
	(*object-key-handler* #'handle-key)
	(*object-value-handler* #'handle-value)
	(*end-of-object-handler* #'handle-end-object))
    (decode-json-from-string str)))

(defun json (&rest args)
  (let ((plist (if (typep (car args) 'list) (car args) args)))
    (funcall #'json:encode-json-plist-to-string plist)))
