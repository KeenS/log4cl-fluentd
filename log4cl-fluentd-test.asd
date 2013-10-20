#|
  This file is a part of log4cl-fluentd project.
  Copyright (c) 2013 κeen
|#

(in-package :cl-user)
(defpackage log4cl-fluentd-test-asd
  (:use :cl :asdf))
(in-package :log4cl-fluentd-test-asd)

(defsystem log4cl-fluentd-test
  :author "κeen"
  :license "BSD"
  :depends-on (:log4cl-fluentd
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "log4cl-fluentd"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
