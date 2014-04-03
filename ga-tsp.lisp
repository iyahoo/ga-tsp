(load "graph-util")

(defparameter *city-number* nil)
(defparameter *edge-num* nil)
(defparameter *max-distance-num* nil)
(defparameter *salesman-num* nil)

(defun init-status ()
  (defparameter *city-number* 10)
  (defparameter *edge-num* 50)
  (defparameter *max-distance-num* 10)
  (defparameter *salesman-num* 10))

(load "make-city")

(defstruct salesman genes fitness distance)

;;(make-human :GENES (gene-code) :fitness 100 :distance 0)

;; calc

(defun gene-code ()
  "重複のないランダムな1-10の遺伝子(数字を街の番号とする)"
  (let ((genes (list (1+ (random *city-number*)))))
    (loop for i
       while (< (length genes) *city-number*)
       do (let ((num (1+ (random *city-number*))))
            (unless (member num genes)
                (push num genes))))
    genes))

(defparameter *salesmans-list* (loop repeat *salesman-NUM*
                                  collect
                                    (make-salesman :GENES (gene-code) :fitness 100 :distance 0)))

;; error
(defun set-distance (salesman edge-alist)
  (let ((genes (salesman-genes salesman))) ;; pop用
    (setf (salesman-distance salesman) (+ (loop for i below *city-number*
                                           sum (get-distance (pop genes) (pop genes) edge-alist))
                                          (get-distance (pop genes) (salesman-genes salesman) edge-alist)))))

(defun calc-distance (salesmans edge-alist)
  (loop for salesman in salesmans
       collect (let (genes (salesman-genes salesman))
                 )))


