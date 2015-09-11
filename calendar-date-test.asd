#|
  This file is a part of calendar-date project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage calendar-date-test-asd
  (:use :cl :asdf))
(in-package :calendar-date-test-asd)

(defsystem calendar-date-test
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:calendar-date
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "calendar-date"))))
  :description "Test system for calendar-date."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
