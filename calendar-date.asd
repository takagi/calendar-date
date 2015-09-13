#|
  This file is a part of calendar-date project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage calendar-date-asd
  (:use :cl :asdf))
(in-package :calendar-date-asd)

(defsystem calendar-date
  :version "0.1"
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:local-time)
  :components ((:module "src"
                :components
                ((:file "calendar-date"))))
  :description "Calendar date library in Common Lisp."
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
  :in-order-to ((test-op (test-op calendar-date-test))))
