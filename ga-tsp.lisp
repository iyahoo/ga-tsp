(load "graph-util")

;; todo
;; ペアの選択 確率=適用度/全体の選択肢体の適用度の総和
;; 交叉 1点 同じ都市番号は入ってない、
;; 突然変異=どれか2つを交換
;; 一回分の試行をまとめる
;; 

(defparameter *city-number* nil)
(defparameter *max-distance-num* nil)
(defparameter *salesman-num* nil)
(defparameter *min-distance-num* nil)
(defparameter *edge-alist* nil)
(defparameter *salesmans-list* nil)

(defun init-status ()
  (setf *city-number* 15)
  (setf *max-distance-num* 20)
  (setf *salesman-num* 15)
  (setf *min-distance-num* (* *max-distance-num* *max-distance-num*))
  (setf *edge-alist* nil))

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

(defun set-distance (salesman edge-alist)
  (let ((genes (salesman-genes salesman))) ;; pop用
    (setf (salesman-distance salesman) (+ (loop for i from 1 below *city-number*
                                           sum (get-distance (pop genes) (car genes) edge-alist))
                                          (get-distance (pop genes) (car (salesman-genes salesman)) edge-alist)))))

(defun minimum-distance (salesmans)
  (reduce #'min (mapcar #'(lambda (salesman)
                            (salesman-distance salesman))
                        salesmans)))

(defun set-fitness (salesman min-distance)
  (setf (salesman-fitness salesman) (/ (* min-distance 1.0) (salesman-distance salesman))))

;; todo 交叉 適応値による2つの親を選ぶ 部分写像交叉

(defun sum-fitness (salesmans)
  (reduce #'+ (mapcar #'salesman-fitness salesmans)))

(defun calc-probability (salesman sum-fitness)
  (/ (salesman-fitness salesman) sum-fitness))

(defun parents-genes (salesmans)
  "ルーレット方式でランダムに二人の親を選ぶ"
  (let ((sumfit (sum-fitness salesmans)))))

(defun set-crossing (salesman salesmans)
  (let ()))

;; todo 突然変異

(defmacro map-salesmans (f salesmans-list target)
  (let ((salesman (gensym)))
    `(mapc #'(lambda (,salesman)
               (,f ,salesman ,target))
           ,salesmans-list)))

(defun initialization ()
  (setf *edge-alist* (make-city-edges))
  (setf *salesmans-list* (loop repeat *salesman-num*
                                  collect (make-salesman :GENES (gene-code) :fitness 0.0 :distance 0))))

(defun update-world ()
  (map-salesmans set-distance *salesmans-list* *edge-alist*) ; macro
  (setf *min-distance-num* (min *min-distance-num* (minimum-distance *salesmans-list*)))
  (map-salesmans set-fitness *salesmans-list*  *min-distance-num*))
 
