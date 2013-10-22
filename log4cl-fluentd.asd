#|
  This file is a part of log4cl-fluentd project.
  Copyright (c) 2013 κeen
|#

#|
  Author: κeen
|#

(in-package :cl-user)
(defpackage log4cl-fluentd-asd
  (:use :cl :asdf))
(in-package :log4cl-fluentd-asd)

(defsystem log4cl-fluentd
  :version "0.1"
  :author "κeen"
  :license "BSD"
  :depends-on (:log4cl :usocket :cl-messagepack :local-time :cl-json :drakma)
  :components ((:module "src"
                :components
                ((:file "log4cl-fluentd"
			:depends-on ("json-util"))
		 (:file "json-util"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op log4cl-fluentd-test))))
