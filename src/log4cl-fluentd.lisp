#|
  This file is a part of log4cl-fluentd project.
  Copyright (c) 2013 κeen
|#

(in-package :cl-user)
(defpackage log4cl-fluentd
  (:use :cl)
  (:import-from  :usocket
		 :socket-connect
		 :socket-stream)
  (:import-from :log4cl-impl
		:fixed-stream-appender
		:appender-do-append
		:pattern-layout
		:layout-to-stream
		:appender
		:appender-layout
		:appender-stream
		:add-appender
		:layout
		:counting-appender)
  (:import-from :local-time
		:now
		:timestamp-to-unix))
(in-package :log4cl-fluentd)

(defclass socket-appender (fixed-stream-appender counting-appender)
  ((host :initarg :host :initform "localhost")
   (port :initarg :port :initform 24224)
   (%socket)))

(defmethod initialize-instance :after ((appender socket-appender) &rest initargs &key keys)
  (with-slots (host port %socket log4cl-impl::stream) appender
    (setf %socket (socket-connect host port :element-type '(unsigned-byte 8)))
    (setf log4cl-impl::stream (socket-stream %socket))))

(defmethod appender-do-append ((appender socket-appender)
			       logger level logfunc)
  (let ((stream (appender-stream appender))
	(hash (make-hash-table :test 'equal)))
    (with-slots (layout log4cl-impl::%output-since-flush) appender
      (setf (gethash "msg" hash) "hello")
      (mpk:encode-stream
        (list "debug.info" (timestamp-to-unix (now)) hash)
       ;; (with-output-to-string (s)
       ;; 	 (layout-to-stream layout s logger level logfunc))
       stream)
      (setf %output-since-flush t)
      (log4cl-impl::maybe-flush-appender-stream appender stream))
    (values)))

(defvar socket-logger (log:make '(socket-logger)))
(defvar fluentd-appender (make-instance 'socket-appender))
(setf (appender-layout fluentd-appender)
      (make-instance 'pattern-layout
		     :conversion-pattern "[%d{%Y-%m-%dT%H:%M:%S%z}, debug.%P, %m]"))
(add-appender socket-logger fluentd-appender)

(log:info :logger socket-logger  "{\"from\":\"cl\"}")


(defparameter aaa (make-hash-table))
(setf (gethash "aaa" aaa) "bbb")
(mpk:decode (mpk:encode aaa))
