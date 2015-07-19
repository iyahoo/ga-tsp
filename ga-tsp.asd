#|
  This file is a part of ga-tsps project.
  Copyright (c) 2015 iyahoo (s1200191@gmail.com)
|#

#|
  Author: iyahoo (s1200191@gmail.com)
|#

(in-package :cl-user)
(defpackage ga-tsp-asd
  (:use :cl :asdf))
(in-package :ga-tsp-asd)

(defsystem ga-tsp
  :version "0.1"
  :author "iyahoo"
  :license ""
  :depends-on (:trivial-shell
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "ga-tsp")
                 (:file "graph-util"))))
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
  :in-order-to ((test-op (test-op ga-tsp-test))))
