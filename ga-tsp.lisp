;; 遺伝的アルゴリズムによる、巡回セールスマン問題の解(近似値)を求めたいプログラム
;;

(ql:quickload :trivial-shell)

(load "graph-util")

(defparameter *city-number* nil)
(defparameter *max-distance-num* nil)   ;町と町の最大距離(この数値以下でランダム)
(defparameter *salesman-num* nil)
(defparameter *min-distance-num* nil)   ;見つけた巡回経路の最短距離
(defparameter *edge-alist* nil)         ;街のalist
(defparameter *salesmans-list* nil)     ;セールスマンのリスト

(load "make-city")

;; setと名前につい
;; 街を作る。make-adge-alistで作成する。
;; alistのcarがnodeの始点、cdrの要素のcarとcdrがそれぞれ終点と距離
;; 遺伝子は数値のリストを巡回経路とする
;; 適応値 = 自分の経路の距離 / 今までの試行で求めた最短の距離
;; ペアの選択 確率 = 適用度/全体の適用度の総和
;; 交叉 1点 同じ都市番号は入ってはいけないので、部分写像交叉も利用する
;; 突然変異=どれか2つを交換
;; 一回分の試行をまとめる

;; todo
;; v

;; replの作成
;; 実行中に現在の*minは表示できるようにする
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

;; 距離関連

(defun set-distance (salesman edge-alist)
  "遺伝子を巡回経路とした距離"
  (let ((genes (salesman-genes salesman))) ;; pop用
    (setf (salesman-distance salesman) (+ (loop for i from 1 below *city-number*
                                             sum (get-distance (pop genes) (car genes) edge-alist))
                                          (get-distance (pop genes) (car (salesman-genes salesman)) edge-alist)))))

(defun minimum-distance (salesmans)
  "全試行において求まった最小の距離とその経路"
  (let ((mindis (reduce #'min (mapcar #'(lambda (salesman)
                                          (salesman-distance salesman))
                                      salesmans))))
    (list mindis (copy-seq (salesman-genes (find mindis salesmans :key #'salesman-distance))))))

;; todo 遺伝子が同じという判定をつよくする(反転、またはn回ずらすと同じ場合は同じ遺伝子)
;;      引数をとる形に作り直す
(defun get-minimum-distance (minimum-distance-num salesmans)
  "最小の周回距離とその遺伝子を保存する。最小値と同じ別な遺伝子が出た場合それも保存する"
  (let* ((pastmin (car minimum-distance-num))
         (newsales (minimum-distance salesmans))
         (newdis (car newsales))
         (newgen (cdr newsales)))
    (if (> pastmin newdis)
        newsales                
        (if (and (= pastmin newdis) (member newgen minimum-distance-num))
            (append minimum-distance-num newgen)
            minimum-distance-num))))

;; 適応値、確率関連

(defun set-fitness (salesman min-distance)
  "適応値をもとめる"
  (setf (salesman-fitness salesman) (/ (* min-distance 1.0) (salesman-distance salesman))))

(defun sum-fitness (salesmans)
  "全てのセールスマンの適応値の合計"
  (reduce #'+ (mapcar #'salesman-fitness salesmans)))

(defun calc-probability (salesman sum-fitness)
  "ひとりのセールスマンが呼ばれる確率"
  (/ (salesman-fitness salesman) sum-fitness))

(defun set-probability (salesman salesmans)
  "選ばれる確率をセットする。適応値が高い場合(ここでは0.9以上とした)、遺伝子を残しやすい用に2倍に設定する"
  (let* ((sumfit (sum-fitness salesmans))
         (prob (calc-probability salesman sumfit)))
    (setf (salesman-probability salesman) prob)))

;; 交叉関連

;; todo 交叉 部分写像交叉

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
       do (push (let ((pos (position (car rfgen) mgen))) ;; error --- should be a lambda expression loopの書き方に問題がありそう
                  (if pos
                      (progn (pop rfgen)                            ; 捨てる
                             (partial-mapping-cross father mgen (+ pos len) len))   ; ここで部分写像 
                      (pop rfgen)))
                newgen))
    (setf (salesman-genes salesman) newgen)))

;; todo 突然変異 RANDOMに2要素を交換するのみ
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
  (setf *city-number* 20)               ; even
  (setf *max-distance-num* 20)
  (setf *salesman-num* 10)
  (setf *min-distance-num* (list (* *max-distance-num* *city-number*) nil)))

(defun initialization ()
  (init-status)
  ;; (setf *edge-alist* (make-city-edges))
    ; 一旦街を固定用
  (setf *edge-alist* '((1 (2 4) (3 4) (4 6) (5 14) (6 6) (7 11) (8 14) (9 13) (10 1) (11 5) (12 9) (13 5) (14 19) (15 12) (16 9) (17 5) (18 20) (19 13) (20 17)) (2 (1 4) (3 18) (4 11) (5 9) (6 18) (7 8) (8 9) (9 14) (10 3) (11 5) (12 20) (13 18) (14 6) (15 3) (16 2) (17 19) (18 2) (19 2) (20 19)) (3 (1 4) (2 18) (4 14) (5 16) (6 15) (7 3) (8 6) (9 7) (10 5) (11 19) (12 20) (13 8) (14 1) (15 15) (16 3) (17 3) (18 2) (19 8) (20 8)) (4 (1 6) (2 11) (3 14) (5 18) (6 12) (7 8) (8 14) (9 11) (10 14) (11 1) (12 19) (13 3) (14 11) (15 6) (16 19) (17 18) (18 9) (19 13) (20 5)) (5 (1 14) (2 9) (3 16) (4 18) (6 16) (7 1) (8 4) (9 17) (10 12) (11 11) (12 11) (13 7) (14 12) (15 5) (16 15) (17 6) (18 2) (19 10) (20 19)) (6 (1 6) (2 18) (3 15) (4 12) (5 16) (7 20) (8 6) (9 15) (10 18) (11 7) (12 16) (13 17) (14 1) (15 2) (16 11) (17 20) (18 1) (19 19) (20 3)) (7 (1 11) (2 8) (3 3) (4 8) (5 1) (6 20) (8 18) (9 9) (10 15) (11 2) (12 4) (13 18) (14 17) (15 19) (16 16) (17 17) (18 14) (19 17) (20 5)) (8 (1 14) (2 9) (3 6) (4 14) (5 4) (6 6) (7 18) (9 9) (10 12) (11 3) (12 20) (13 14) (14 20) (15 6) (16 15) (17 7) (18 3) (19 5) (20 10)) (9 (1 13) (2 14) (3 7) (4 11) (5 17) (6 15) (7 9) (8 9) (10 14) (11 19) (12 4) (13 7) (14 4) (15 8) (16 12) (17 5) (18 2) (19 5) (20 16)) (10 (1 1) (2 3) (3 5) (4 14) (5 12) (6 18) (7 15) (8 12) (9 14) (11 15) (12 3) (13 13) (14 6) (15 4) (16 14) (17 2) (18 12) (19 10) (20 13)) (11 (1 5) (2 5) (3 19) (4 1) (5 11) (6 7) (7 2) (8 3) (9 19) (10 15) (12 12) (13 6) (14 12) (15 13) (16 20) (17 5) (18 3) (19 9) (20 19)) (12 (1 9) (2 20) (3 20) (4 19) (5 11) (6 16) (7 4) (8 20) (9 4) (10 3) (11 12) (13 9) (14 2) (15 15) (16 15) (17 20) (18 2) (19 16) (20 2)) (13 (1 5) (2 18) (3 8) (4 3) (5 7) (6 17) (7 18) (8 14) (9 7) (10 13) (11 6) (12 9) (14 10) (15 18) (16 6) (17 9) (18 11) (19 16) (20 13)) (14 (1 19) (2 6) (3 1) (4 11) (5 12) (6 1) (7 17) (8 20) (9 4) (10 6) (11 12) (12 2) (13 10) (15 1) (16 20) (17 11) (18 2) (19 1) (20 19)) (15 (1 12) (2 3) (3 15) (4 6) (5 5) (6 2) (7 19) (8 6) (9 8) (10 4) (11 13) (12 15) (13 18) (14 1) (16 3) (17 19) (18 3) (19 17) (20 3)) (16 (1 9) (2 2) (3 3) (4 19) (5 15) (6 11) (7 16) (8 15) (9 12) (10 14) (11 20) (12 15) (13 6) (14 20) (15 3) (17 13) (18 15) (19 2) (20 3)) (17 (1 5) (2 19) (3 3) (4 18) (5 6) (6 20) (7 17) (8 7) (9 5) (10 2) (11 5) (12 20) (13 9) (14 11) (15 19) (16 13) (18 12) (19 9) (20 15)) (18 (1 20) (2 2) (3 2) (4 9) (5 2) (6 1) (7 14) (8 3) (9 2) (10 12) (11 3) (12 2) (13 11) (14 2) (15 3) (16 15) (17 12) (19 16) (20 1)) (20 (1 17) (2 19) (3 8) (4 5) (5 19) (6 3) (7 5) (8 10) (9 16) (10 13) (11 19) (12 2) (13 13) (14 19) (15 3) (16 3) (17 15) (18 1) (19 20)) (19 (1 13) (2 2) (3 8) (4 13) (5 10) (6 19) (7 17) (8 5) (9 5) (10 10) (11 9) (12 16) (13 16) (14 1) (15 17) (16 2) (17 9) (18 16) (20 20))))
  
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
  (format t "q: quit c: check-stat number: update-world n times")
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

(defun main-body ()
  (initialization)
  (repl))
