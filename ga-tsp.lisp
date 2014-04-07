(load "graph-util")

;; todo
;; 遺伝子コードを決める
;; 各個体の適応度を決める
;; 適応度=コレまで求めた最小値/その個体の距離
;; ペアの選択 確率=適用度/全体の選択肢体の適用度の総和
;; 交叉 1点 同じ都市番号は入ってない、
;; 突然変異=どれか2つを交換


(defparameter *city-number* nil)
(defparameter *edge-num* nil)
(defparameter *max-distance-num* nil)
(defparameter *salesman-num* nil)

(defun init-status ()
  (defparameter *city-number* 10)
  (defparameter *edge-num* 50)
  (defparameter *max-distance-num* 10)
  (defparameter *salesman-num* 10)
  (defparameter *min-distance-num* (* *max-distance-num* *max-distance-num*)))

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
                                  collect (make-salesman :GENES (gene-code) :fitness 0.0 :distance 0)))

(defun set-distance (salesman edge-alist)
  (let ((genes (salesman-genes salesman))) ;; pop用
    (setf (salesman-distance salesman) (+ (loop for i from 1 below *city-number*
                                           sum (get-distance (pop genes) (car genes) edge-alist))
                                          (get-distance (pop genes) (car (salesman-genes salesman)) edge-alist)))))

(defun get-minimum-distance (salesmans)
  (reduce #'min (mapcar #'(lambda (salesman)
                            (salesman-distance salesman))
                        salesmans)))

(defun set-fitness (salesman min-distance)
  (setf (salesman-fitness salesman) (/ (* min-distance 1.0) (salesman-distance salesman))))

;; (defun update-world ()
;;   (mapc #'(lambda (salesman)
;;             (set-distance salesman *edge-alist*))
;;         *salesman-num*)

;;   (setf *min-distance-num* (get-minimum-distance *salesmans-list*))
  
;;   (mapc #'(lambda (salesman)
;;             (set-fitness salesman *edge-alist*))
;;         *salesmans-list*))

