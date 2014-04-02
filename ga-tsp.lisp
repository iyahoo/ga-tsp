(load "graph-util")
(compile-file "make-city")
(load "make-city")

(defconstant +edge-num+ 50)
(defconstant +max-distance-num+ 10)
(defconstant +salesman-num+ 10)

(defstruct salesman genes fitness distance)

;; for test

(defparameter *saleseman-list* (loop repeat +salesman-num+
                                    collect (make-human :GENES (gene-code) :fitness 100 :distance 0)))

;;(make-human :GENES (gene-code) :fitness 100 :distance 0)

;; calc

(defun gene-code ()
  "重複のないランダムな1-10の遺伝子をつくる"
  (let ((genes (list (1+ (random 10)))))
    (loop for i
       while (< (length genes) 10)
       do (let ((num (1+ (random 10))))
            (when (and (not (member num genes))
                       (member num
                               (mapcar #'car
                                        (cdr (assoc (car genes) *edge-alist*)))))
                (push num genes))))
    genes))

(defun calc-distance (salesemans) ())
