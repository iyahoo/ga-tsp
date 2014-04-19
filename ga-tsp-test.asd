#|
  This file is a part of ga-tsp project.
  Copyright (c) 2014 iyahoo (lhcpr191)
|#

(in-package :cl-user)
(defpackage ga-tsp-test-asd
  (:use :cl :asdf))
(in-package :ga-tsp-test-asd)

(defsystem ga-tsp-test
  :author "iyahoo"
  :license ""
  :depends-on (:ga-tsp
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "ga-tsp"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
