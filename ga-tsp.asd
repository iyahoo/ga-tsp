#|
  This file is a part of ga-tsp project.
  Copyright (c) 2014 iyahoo (lhcpr191)
|#

#|
  Author: iyahoo (lhcpr191)
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
                 (:file "make-city")
                 (:file "graph-util")
                 (:file "head"))))
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
  :in-order-to ((test-op (load-op ga-tsp-test))))