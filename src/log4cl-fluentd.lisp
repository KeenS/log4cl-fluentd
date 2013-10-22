#|
  This file is a part of log4cl-fluentd project.
  Copyright (c) 2013 κeen
|#

(in-package :cl-user)
(defpackage log4cl-fluentd
  (:use :cl :log4cl-impl)
  (:import-from :usocket
		:socket-connect
		:socket-stream)
  (:import-from :local-time
		:now
		:timestamp-to-unix)
  (:import-from :log4cl-fluentd.json
		:decode-json-to-hash
		:json)
  (:import-from :log4cl-impl
		:logger
		:name-format
		:backup-name-format
		:enabled
		:call-user-log-message)
  (:export :fluentd-logger
	   :socket-appender
	   :host
	   :port
	   :json
	   :logger
	   :fluentd-forward-appender
	   :fluentd-http-appender
	   :fluentd-tail-appender
	   :forward-appender
	   :http-appender
	   :tail-appender
	   :create-fluentd-logger))
(in-package :log4cl-fluentd)

(defclass socket-appender (fixed-stream-appender)
  ((host :initarg :host :initform "localhost" :accessor host)
   (port :initarg :port :initform 24224 :accessor port)
   (%socket :initarg :socket)))

(defmethod initialize-instance :after ((appender socket-appender) &rest args &key keys)
  (with-slots (host port %socket stream) appender
    (setf %socket (socket-connect host port :element-type '(unsigned-byte 8)))
    (setf stream (socket-stream %socket))))

(defclass fluentd-appender (appender)
  ())

(defclass fluentd-forward-appender (socket-appender fluentd-appender)
  ())
(defmethod property-alist ((appender fluentd-forward-appender))
  (append (call-next-method)
	  '((:forward enabled :boolean)
	    (:forward-port port :number)
	    (:forward-host host :string))))

(defmethod appender-do-append ((appender fluentd-forward-appender)
			       logger level logfunc)
  (with-slots (layout log4cl-impl::%output-since-flush) appender
    (let ((stream (appender-stream appender))
	  (json (with-output-to-string (s)
		  (funcall logfunc s))))
      (mpk:encode-stream
       (list "debug.tag" (timestamp-to-unix (now)) (decode-json-to-hash json))
       stream)
      (setf %output-since-flush t)
      (log4cl-impl::maybe-flush-appender-stream appender stream)
      (values))))

(defclass http-appender (appender)
  ((host :initarg :host :initform "localhost" :accessor host)
   (port :initarg :port :initform 80 :accessor port)))

(defclass fluentd-http-appender (http-appender fluentd-appender)
  ((port :initform 9880)))

(defmethod property-alist ((appender fluentd-http-appender))
  (append (call-next-method)
	  '((:http enabled :boolean)
	    (:http-port port :number)
	    (:http-host host :string))))

(defmethod appender-do-append ((appender fluentd-http-appender)
			       logger level logfunc)
    (let ((tag "debug.tag")
	  (json (with-output-to-string (s) (funcall logfunc s))))
     (with-slots (host port) appender
       (drakma:http-request (format nil "http://~a:~D/~:[~;~:*~A~]" host port tag)
			    :method :post
			    :parameters (list (cons "json" json))))))

(defclass fluentd-tail-appender (daily-file-appender fluentd-appender)
  ((layout :initform (make-instance 'pattern-layout :conversion-pattern "%m%n"))
   (name-format :initform "log4cl-fluentd.log")
   (backup-name-format :initform "")))
(defmethod property-alist ((appender fluentd-http-appender))
  (append (call-next-method)
	  '((:tail enabled :boolean)
	    (:tail-file-path name-format :string)
	    (:tail-backup-format backup-name-format :string))))

(defvar fluentd-logger)
(defvar forward-appender)
(defvar http-appender)
(defvar tail-appender)

(defun create-fluentd-logger (&key
				forward forward-port forward-host
				http http-port http-host
				tail tail-name-format tail-backup-format
				keys)
  (let ((logger (log:category '(fluentd))))
    (when forward
      (setf forward-appender
	    (apply #'make-instance `(fluentd-forward-appender
				     ,@(if forward-port `(:port ,forward-port))
				     ,@(if forward-host `(:host ,forward-host)))))
      (add-appender logger forward-appender))
    (when http
      (setf http-appender
	    (apply #'make-instance `(fluentd-http-appender
				     ,@(if http-port `(:port ,http-port))
				     ,@(if http-host `(:host ,http-host)))))
      (add-appender logger http-appender))
    (when tail
      (setf tail-appender
	    (apply #'make-instance `(fluentd-tail-appender
				     ,@(if tail-name-format `(:name-format ,tail-name-format))
				     ,@(if tail-backup-format `(:backup-name-format ,tail-backup-format)))))
      (add-appender logger tail-appender))
    (set-log-level logger +log-level-info+)
    (setf fluentd-logger logger)))
