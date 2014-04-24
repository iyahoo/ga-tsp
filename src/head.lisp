#|
  This file is a part of ga-tsp project.
  Copyright (c) 2014 iyahoo (lhcpr191@gmail.com)
|#

(in-package :cl-user)
(defpackage ga-tsp
  (:use :cl))
(in-package :ga-tsp)

(defparameter *city-number* nil)
(defparameter *max-distance-num* nil)   ;町と町の最大距離(この数値以下でランダム)
(defparameter *salesman-num* nil)
(defparameter *min-distance-num* nil)   ;見つけた巡回経路の最短距離
(defparameter *edge-alist* nil)         ;街のalist
(defparameter *salesmans-list* nil)
