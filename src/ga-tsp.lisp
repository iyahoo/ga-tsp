(in-package :cl-user)
(defpackage ga-tsp
  (:use :cl))
(in-package :ga-tsp)

(cl-annot:enable-annot-syntax)

;; You have to (initialize) 

(defvar *city-number* 10)
(defvar *max-distance* 20)
(defvar *num-of-salesman* 10)
(defvar *min-distance* (list (* *max-distance* *city-number*) nil))

(defparameter *edges-alist* nil)
(defparameter *salesmans-list* nil)

(defclass salesman ()
  ((genes :accessor salesman-genes :initarg :genes)
   (distance :accessor salesman-distance :initarg :distance)
   (fitness :accessor salesman-fitness :initarg :fitness)
   (probability :accessor salesman-probability :initarg :probability)))

(defmethod print-object ((obj salesman) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "genes: ~a distance: ~a fitness: ~a probability ~a"
            (salesman-genes obj) (salesman-distance obj) (salesman-fitness obj) (salesman-probability obj))))

(defun print-salesmans ()
  (map 'list #'(lambda (salesman)
                 (format nil "~a~%" salesman))
       *salesmans-list*))

(defun gene-code (&optional genes)
  "Generate not duplicatively genes"
  (assert (<= (length genes) *city-number*))
  (if (= (length genes) *city-number*)
      genes
      (let ((new-gene (1+ (random *city-number*))))
        (gene-code (adjoin new-gene genes)))))

(defun create-salesman ()
  (let* ((genes (gene-code))
         (distance (get-all-distance genes)))
    (make-instance 'salesman :genes genes :distance distance :fitness 0 :probability 0)))

(defun make-1-n-list (n &optional acc)
  (assert (>= n 0))
  (if (= n 0)
      acc
      (make-1-n-list (1- n) (cons n acc))))

(defun edges-alist ()
  (let ((nodes (make-1-n-list *city-number*)))
    (mapcar
     #'(lambda (node-head)
         (cons node-head
               (mapcar #'(lambda (edge)
                           (list edge))
                       (remove node-head nodes))))
     nodes)))

(defun delete-duplicates-edges (edge)
  (mapcar #'(lambda (edge1)
              (let ((head (first edge1))
                    (rest (rest edge1)))
                (cons head (remove-if #'(lambda (e) (> head (car (last e)))) rest))))
          edge))

(defun add-distance (edge-alist)
  (mapcar #'(lambda (edge)
              (let ((node-head (first edge))
                    (node-rest (rest edge)))
                (cons node-head
                      (mapcar #'(lambda (node-list)
                                  (let ((node1 (car node-list)))
                                    (list node1 (if (< node-head node1)
                                                    (1+ (random *max-distance*))
                                                    nil))))
                              node-rest))))
          edge-alist))

(defun get-distance (start end)
  "Distance from start to end"
  (assert (and (/= start end) (>= *city-number* start 1) (>= *city-number* end 1)))
  (let ((s start)
        (e end))
    (when (> start end)
      (setf s end)
      (setf e start))
    (car (cdr (find e (cdr (assoc s *edges-alist*)) :key #'car)))))

(defun get-all-distance (genes &optional start end (total 0) first)
  "Sum of all city distance (genes)"
  (when (and (null start) (null end) (null first))
    (setf start (first genes)
          end (second genes)
          first start))
  (if end
      (get-all-distance (rest genes) (second genes) (third genes)
                        (+ total (get-distance start end)) first)
      (+ total (get-distance start first))))

(defun minimum-distance ()
  (%minimum-distance *salesmans-list*))

;; for test
(defun %minimum-distance (salesmans)
  "return pair of minimum-distance and genes"
  (let ((mindist (reduce #'min (mapcar #'(lambda (salesman)
                                           (salesman-distance salesman))
                                       salesmans))))
    (list mindist (salesman-genes (find mindist salesmans :key #'salesman-distance)))))

(defmethod calc-fitness ((salesman salesman))
  (/ (float (car (minimum-distance))) (salesman-distance salesman)))

(defun sum-fitness (salesmans)
  (reduce #'+ (mapcar #'salesman-fitness salesmans)))

(defmethod calc-probability ((salesman salesman))
  "Probability of be selected when selecting parent genes"
  (/ (salesman-fitness salesman) (sum-fitness *salesmans-list*)))

(defun initialize ()
  (setf *edges-alist* (add-distance (delete-duplicates-edges (edges-alist)))
        *salesmans-list* (loop :repeat *num-of-salesman*
                            :collect (create-salesman)))
  (loop :for salesman :in *salesmans-list*
     :collect (setf (salesman-fitness salesman) (calc-fitness salesman)))
  (loop :for salesman :in *salesmans-list*
     :collect (setf (salesman-probability salesman) (calc-probability salesman))))

(defun test ()
  (asdf:test-system :ga-tsp))

;; --- fix 2015/2/15

(defun parents-genes (salesmans)
  "ルーレット方式でランダムに一人の親を選び遺伝子コードを返す"
  (let ((decision (random 1.00)))
    (loop for i from 0 to (1- *num-of-salesman*)
       do (when (< (setf decision (- decision (salesman-probability (nth i salesmans)))) 0)
            (return (salesman-genes (nth i salesmans)))))))

;; father 写像用
;; mgen 同じものが入っているかどうかはnewgenでなく、mgenを見ればよい
;; addlen mgenは後ろの半分なので、要素数を求めたらこの要素数を加える
;; pos 前にpopを試みたがすでに入っていた要素番号
;; partially-mapped-crossover
(defun partially-mapped-crossover (father mgen pos addlen)
  "部分写像交叉"
  (let* ((addgen (nth pos father))
         (overpos (position addgen mgen))) ; Overlap position
    (if overpos
        (partially-mapped-crossover father mgen (+ overpos addlen) addlen)
        addgen)))

(defun set-crossing (salesman c-salesmans) ; コピーしたものを渡す
  "交叉の実行"
  (let* ((father (parents-genes c-salesmans))
         (mother (parents-genes c-salesmans))
         (len (/ (length father) 2))
         (rfgen (copy-seq (nthcdr len (reverse father))))                  ; 父となる遺伝子の逆順になった前半分
         (mgen (copy-seq (nthcdr len mother)))
         (newgen (copy-seq mgen)))
    (loop for i below len
       do (push (let ((pos (position (car rfgen) mgen)))
                  (if pos
                      (progn (pop rfgen)                            ; 捨てる
                             (partially-mapped-crossover father mgen (+ pos len) len))   ; ここで部分写像
                      (pop rfgen)))
                newgen))
    (setf (salesman-genes salesman) newgen)))

;; 突然変異
(defun set-mutation (salesman)
  (let* ((len (length (salesman-genes salesman)))
         (randpos1 (random (1- len)))
         (randpos2 (random (1- len))))
    (rotatef (nth randpos1 (salesman-genes salesman))
             (nth randpos2 (salesman-genes salesman)))))

;; 初期化

(defmacro map-salesmans (f salesmans-list target)
  (let ((salesman (gensym)))
    `(mapc #'(lambda (,salesman)
               (,f ,salesman ,target))
           ,salesmans-list)))

(defun initialization ()
  (init-status)
  (setf *edge-alist* (make-city-edges))

  (setf *salesmans-list* (loop repeat *num-of-salesman*
                            collect (make-salesman :GENES (gene-code) :fitness 0.0 :distance 0 :probability 0)))

  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  (setf *min-distance* (get-minimum-distance *min-distance* *salesmans-list*))
  (map-salesmans set-fitness *salesmans-list*  (car *min-distance*))
  (map-salesmans set-probability *salesmans-list* *salesmans-list*))

;; 実行部

(defun check-stat ()
  (format nil "min: ~a ~a ~% salesman: ~%~{~t~a~%~}" (car *min-distance*) (cdr *min-distance*) *salesmans-list*))

(defun update-world ()
  "この関数を入力した値分くりかえし、近似を求める"
  (map-salesmans set-crossing *salesmans-list* (copy-seq *salesmans-list*))
  (mapcar #'(lambda (salesman)
              (set-mutation salesman))
          *salesmans-list*)
  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  (setf *min-distance* (get-minimum-distance *min-distance* *salesmans-list*))
  (map-salesmans set-fitness *salesmans-list*  (car *min-distance*))
  (map-salesmans set-probability *salesmans-list* *salesmans-list*)
  (check-stat))

(defun repl ()
  (fresh-line)
  (format t "q: quit, c: check-stat, number: update-world n times ~%> ")
  (let ((input (read-line)))
    (labels ((check ()
               (format t "~a" (check-stat))))
      (cond ((equal input "q") ())
            ((equal input "c")
             (check)
             (repl))
            (t (let ((n (parse-integer input :junk-allowed t)))
                 (if n
                     (loop for i below n
                        do (update-world)
                        if (zerop (mod i 5000))
                        do (princ #\.))
                     (update-world))
                 (fresh-line)
                 (check)
                 (repl)))))))

@export
(defun main ()
  (initialization)
  (repl))


