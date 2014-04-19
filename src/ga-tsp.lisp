#|
  This file is a part of ga-tsp project.
  Copyright (c) 2014 iyahoo (lhcpr191)
|#

(in-package :cl-user)
(defpackage ga-tsp
  (:use :cl))
(in-package :ga-tsp)

;; 遺伝的アルゴリズムによる、巡回セールスマン問題の解(近似値)を求めたいプログラム
;;

;; city-alistのcarがnodeの始点、cdrの要素のcarとcdrがそれぞれ終点と距離
;; 遺伝子は数値のリストを巡回経路とする
;; 適応値 = 自分の経路の距離 / 今までの試行で求めた最短の距離
;; ペアの選択 確率 = 適用度/全体の適用度の総和
;; 交叉 1点 同じ都市番号は入ってはいけないので、部分写像交叉も利用する
;; 突然変異=どれか2つを交換


;; todo
;; v

;; コードを綺麗に
;; asdを作る

(defstruct salesman genes fitness distance probability)

(defun gene-code ()
  "重複のないランダムな1~*city-number*の遺伝子(数字を街の番号とする)"
  (let ((genes (list (1+ (random *city-number*)))))
    (loop 
       while (< (length genes) *city-number*)
       do (let ((num (1+ (random *city-number*))))
            (unless (member num genes)
              (push num genes))))
    genes))

;; 距離

(defun set-distance (salesman edge-alist)
  "遺伝子を巡回経路とした距離"
  (let ((genes (salesman-genes salesman))) ;; pop用
    (setf (salesman-distance salesman) (+ (loop for i from 1 below *city-number*
                                             sum (get-distance (pop genes) (car genes) edge-alist))
                                          (get-distance (pop genes) (car (salesman-genes salesman)) edge-alist)))))

(defun min-distance-list (salesmans)
  "全試行において求まった最小の距離とその経路"
  (let ((mindis (reduce #'min (mapcar #'(lambda (salesman)
                                          (salesman-distance salesman))
                                      salesmans))))
    (list mindis (copy-seq (salesman-genes (find mindis salesmans :key #'salesman-distance))))))

;; todo 遺伝子が同じという判定をつよくする(反転、またはn回ずらすと同じ場合は同じ遺伝子)
(defun get-minimum-distance (minimum-distance-num salesmans)
  "最小の周回距離とその遺伝子を保存する。最小値と同じ別な遺伝子が出た場合それも保存する"
  (let* ((pastmin (car minimum-distance-num))
         (newsales (min-distance-list salesmans))
         (newdis (car newsales))
         (newgen (cdr newsales)))
    (if (> pastmin newdis)
        newsales                
        (if (and (= pastmin newdis) (member newgen minimum-distance-num))
            (append minimum-distance-num newgen)
            minimum-distance-num))))

;; 適応値

(defun set-fitness (salesman min-distance)
  "適応値をもとめる"
  (setf (salesman-fitness salesman) (/ (* min-distance 1.0) (salesman-distance salesman))))

(defun sum-fitness (salesmans)
  "全てのセールスマンの適応値の合計"
  (reduce #'+ (mapcar #'salesman-fitness salesmans)))

;; 交叉

(defun calc-probability (salesman sum-fitness)
  "ひとりのセールスマンが呼ばれる確率"
  (/ (salesman-fitness salesman) sum-fitness))

(defun set-probability (salesman salesmans)
  "選ばれる確率をセットする。"
  (let* ((sumfit (sum-fitness salesmans))
         (prob (calc-probability salesman sumfit)))
    (setf (salesman-probability salesman) prob)))

;; 1.0から順に自分の確率で減らしていき、0を切った時に選ばれたとする
(defun parents-genes (salesmans)
  "ルーレット方式でランダムに一人の親を選び遺伝子コードを返す"
  (let ((decision (random 1.00)))
    (loop for i from 0 to (1- *salesman-num*)
       do (when (< (setf decision (- decision (salesman-probability (nth i salesmans)))) 0)
                     (return (salesman-genes (nth i salesmans)))))))

;; 必要な情報 リストのサイズ newgen 入れたい値(最初はset-crossingより)
;; father 写像用
;; mgen 同じものが入っているかどうかはnewgenでなく、mgenを見ればよい
;; addlen mgenは後ろの半分なので、要素数を求めたらこの要素数を加える
;; pos 前にpopを試みたがすでに入っていた要素番号

(defun partial-mapping-cross (father mgen pos addlen)  
  "部分写像交叉"
  (let* ((addgen (nth pos father))
         (overpos (position addgen mgen))) ; Overlap position
    (if overpos
        (partial-mapping-cross father mgen (+ overpos addlen) addlen)
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
                             (partial-mapping-cross father mgen (+ pos len) len))   ; ここで部分写像 
                      (pop rfgen)))
                newgen))
    (setf (salesman-genes salesman) newgen)))

;; 突然変異
(defun set-mutation (salesman)
  (let* ((genes (salesman-genes salesman))
         (len (length genes))
         (randpos1 (random (1- len)))
         (randpos2 (random (1- len)))
         (buf (nth randpos1 genes)))
    (setf (nth randpos1 (salesman-genes salesman)) (nth randpos2 (salesman-genes salesman)))
    (setf (nth randpos2 (salesman-genes salesman)) buf)))

;; 初期化

(defmacro map-salesmans (f salesmans-list target)
  (let ((salesman (gensym)))
    `(mapc #'(lambda (,salesman)
               (,f ,salesman ,target))
           ,salesmans-list)))

(defun init-status ()
  (setf *city-number* 10)               ; even
  (setf *max-distance-num* 20)
  (setf *salesman-num* 5)
  (setf *min-distance-num* (list (* *max-distance-num* *city-number*) nil)))

(defun initialization ()
  (init-status)
  ;; (setf *edge-alist* (make-city-edges))
    ; 街を固定
  (setf *edge-alist* '((1 (2 8) (3 13) (4 7) (5 16) (6 6) (7 6) (8 1) (9 18) (10 2))
                       (2 (1 8) (3 7) (4 4) (5 4) (6 3) (7 8) (8 17) (9 8) (10 17))
                       (3 (1 13) (2 7) (4 11) (5 9) (6 12) (7 8) (8 19) (9 19) (10 17))
                       (4 (1 7) (2 4) (3 11) (5 6) (6 17) (7 20) (8 6) (9 11) (10 14))
                       (5 (1 16) (2 4) (3 9) (4 6) (6 4) (7 16) (8 1) (9 2) (10 12))
                       (6 (1 6) (2 3) (3 12) (4 17) (5 4) (7 14) (8 16) (9 13) (10 10))
                       (7 (1 6) (2 8) (3 8) (4 20) (5 16) (6 14) (8 9) (9 13) (10 3))
                       (8 (1 1) (2 17) (3 19) (4 6) (5 1) (6 16) (7 9) (9 17) (10 19))
                       (10 (1 2) (2 17) (3 17) (4 14) (5 12) (6 10) (7 3) (8 19) (9 12))
                       (9 (1 18) (2 8) (3 19) (4 11) (5 2) (6 13) (7 13) (8 17) (10 12))))
  
  (setf *salesmans-list* (loop repeat *salesman-num*
                            collect (make-salesman :GENES (gene-code) :fitness 0.0 :distance 0 :probability 0)))
  
  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  (setf *min-distance-num* (get-minimum-distance *min-distance-num* *salesmans-list*))
  (map-salesmans set-fitness *salesmans-list*  (car *min-distance-num*))
  (map-salesmans set-probability *salesmans-list* *salesmans-list*))

;; 実行部

(defun check-stat ()
  (format nil "min: ~a ~a ~% salesman: ~%~{~t~a~%~}" (car *min-distance-num*) (cdr *min-distance-num*) *salesmans-list*))

(defun update-world ()
  "この関数を入力した値分くりかえし、近似を求める"

  (map-salesmans set-crossing *salesmans-list* (copy-seq *salesmans-list*))

  (mapcar #'(lambda (salesman)
              (set-mutation salesman))
          *salesmans-list*)
  
  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  
  (setf *min-distance-num* (get-minimum-distance *min-distance-num* *salesmans-list*))
  
  (map-salesmans set-fitness *salesmans-list*  (car *min-distance-num*))

  (map-salesmans set-probability *salesmans-list* *salesmans-list*)

  (check-stat))

(defun repl ()
  (fresh-line)
  (format t "q: quit, c: check-stat, number: update-world n times")
  (fresh-line)
  (format t "> ")
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

(defun main ()
  (initialization)
  (repl))
