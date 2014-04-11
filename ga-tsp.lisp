;; 遺伝的アルゴリズムによる、巡回セールスマン問題の解(近似値)を求めたいプログラム
;; 

(load "graph-util")

(defparameter *city-number* nil)
(defparameter *max-distance-num* nil)   ;町と町の最大距離(この数値以下でランダム)
(defparameter *salesman-num* nil)
(defparameter *min-distance-num* nil)   ;見つけた巡回経路の最短距離
(defparameter *edge-alist* nil)         ;街のalist
(defparameter *salesmans-list* nil)     ;セールスマンのリスト

(load "make-city")

;; setと名前についた関数は構造体に変更を加える
;; 街を作る。make-adge-alistで作成する。
;; alistのcarがnodeの始点、cdrの要素のcarとcdrがそれぞれ終点と距離
;; 遺伝子は数値のリストを巡回経路とする
;; 適応値 = 自分の経路の距離 / 今までの試行で求めた最短の距離
;; ペアの選択 確率 = 適用度/全体の適用度の総和
;; 交叉 1点 同じ都市番号は入ってはいけないので、部分写像交叉も利用する
;; 突然変異=どれか2つを交換
;; 一回分の試行をまとめる

;; todo

;; replの作成
;; 実行中に現在の*minは表示できるようにする
;; asdを作る

(defstruct salesman genes fitness distance probability)

(defun gene-code ()
  "重複のないランダムな1~*city-number*の遺伝子(数字を街の番号とする)"
  (let ((genes (list (1+ (random *city-number*)))))
    (loop for i
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

;; (if (> prob 0.9)
;;     (* prob 2)
;;     prob)
;; 交叉関連

;; todo 交叉 部分写像交叉

;; から順に自分の確率で減らしていき、0を切った時に選ばれたとする
(defun parents-genes (salesmans)
  "ルーレット方式でランダムに一人の親を選び遺伝子コードを返す"
  (let ((decision (random 1.00)))
    (loop for i do (when (< (setf decision (- decision (salesman-probability (nth i salesmans)))) 0)
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

(defun init-status ()
  (setf *city-number* 10)               ; even
  (setf *max-distance-num* 20)
  (setf *salesman-num* 10)
  (setf *min-distance-num* (list (* *max-distance-num* *city-number*) nil)))

(defun initialization ()
  (init-status)
  (setf *edge-alist* (make-city-edges))
  (setf *salesmans-list* (loop repeat *salesman-num*
                            collect (make-salesman :GENES (gene-code) :fitness 0.0 :distance 0 :probability 0)))
  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  (setf *min-distance-num* (if (> (car *min-distance-num*) (car (minimum-distance *salesmans-list*)))
                               (minimum-distance *salesmans-list*)
                               *min-distance-num*))
  (map-salesmans set-fitness *salesmans-list*  (car *min-distance-num*))
  (map-salesmans set-probability *salesmans-list* *salesmans-list*))

;; 実行部

(defun update-world ()
  "この関数を入力した値分くりかえし、近似を求める"

  (map-salesmans set-crossing *salesmans-list* (copy-seq *salesmans-list*))

  (mapcar #'(lambda (salesman)
              (set-mutation salesman))
          *salesmans-list*)
  
  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  
  (setf *min-distance-num* (if (> (car *min-distance-num*) (car (minimum-distance *salesmans-list*)))
                               (minimum-distance *salesmans-list*)
                               *min-distance-num*))
  
  (map-salesmans set-fitness *salesmans-list*  (car *min-distance-num*))

  (map-salesmans set-probability *salesmans-list* *salesmans-list*))

(defun check-stat ()
  (format nil "min: ~a ~a ~% salesman: ~a ~%" (car *min-distance-num*) (cdr *min-distance-num*) *salesmans-list*))

(defun repl ())

;; マクロ

(defmacro map-salesmans (f salesmans-list target)
  (let ((salesman (gensym)))
    `(mapc #'(lambda (,salesman)
               (,f ,salesman ,target))
           ,salesmans-list)))

