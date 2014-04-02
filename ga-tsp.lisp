(load "graph-util")

(defparameter *city-number* 10)
(defparameter *edge-num* 50)
(defparameter *max-distance-num* 10)
(defparameter *salesman-num* 10)

(load "make-city")

(defstruct salesman genes fitness distance)

;;(make-human :GENES (gene-code) :fitness 100 :distance 0)

;; calc

(defun gene-code ()
  "重複のないランダムな1-10の遺伝子(数字を街の番号とする)"
  (let ((genes (list (1+ (random 10)))))
    (loop for i
       while (< (length genes) 10)
       do (let ((num (1+ (random 10))))
            (unless (member num genes)
                (push num genes))))
    genes))

(defparameter *salesmans-list* (loop repeat *salesman-NUM*
                                  collect
                                    (make-salesman :GENES (gene-code) :fitness 100 :distance 0)))

;; (defun calc-distance (salesmans edge-alist)
;;   (loop for salesman in salesmans
;;        collect (let (genes (salesman-genes salesman))
;;                  )))
