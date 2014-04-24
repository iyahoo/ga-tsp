#|
  This file is a part of ga-tsp project.
  Copyright (c) 2014 iyahoo (lhcpr191@gmail.com)
|#

#|
  Author: iyahoo (lhcpr191@gmail.com)
|#

(in-package :cl-user)
(defpackage ga-tsp-asd
  (:use :cl :asdf))
(in-package :ga-tsp-asd)

(defsystem ga-tsp
  :version "0.1"
  :author "iyahoo"
  :license "public"
  :depends-on (:trivial-shell
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "head")
                 (:file "graph-util")
                 (:file "ga-tsp" :depends-on ("head"))
                 (:file "make-city" :depends-on ("ga-tsp")))))
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
