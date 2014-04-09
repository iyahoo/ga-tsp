;; 遺伝的アルゴリズムによる、巡回セールスマン問題の解(近似値)を求めるプログラム
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
;; replの作成

(defstruct salesman genes fitness distance probability)

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
  "遺伝子を巡回経路とした距離"
  (let ((genes (salesman-genes salesman))) ;; pop用
    (setf (salesman-distance salesman) (+ (loop for i from 1 below *city-number*
                                           sum (get-distance (pop genes) (car genes) edge-alist))
                                          (get-distance (pop genes) (car (salesman-genes salesman)) edge-alist)))))

(defun minimum-distance (salesmans)
  "全試行において求まった最小の距離"
  (reduce #'min (mapcar #'(lambda (salesman)
                            (salesman-distance salesman))
                        salesmans)))

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
  (let ((sumfit (sum-fitness salesmans)))
    (setf (salesman-probability salesman) (calc-probability salesman sumfit))))

;; todo 交叉 確率に応じて2つの親を選ぶ 部分写像交叉

;; 合計を出す 
(defun parents-genes (salesmans)
  "ルーレット方式でランダムに二人の親を選ぶ")

(defun set-crossing (salesman salesmans)
  "交叉の実行")

;; todo 突然変異
(defun set-mutation (salesman))

(defun init-status ()
  (setf *city-number* 15)
  (setf *max-distance-num* 20)
  (setf *salesman-num* 10)
  (setf *min-distance-num* (* *max-distance-num* *max-distance-num*))
  (setf *edge-alist* nil))

(defun initialization ()
  (init-status)
  (setf *edge-alist* (make-city-edges))
  (setf *salesmans-list* (loop repeat *salesman-num*
                                  collect (make-salesman :GENES (gene-code) :fitness 0.0 :distance 0 :probability 0))))

(defmacro map-salesmans (f salesmans-list target)
  (let ((salesman (gensym)))
    `(mapc #'(lambda (,salesman)
               (,f ,salesman ,target))
           ,salesmans-list)))

;; todo 交叉関数と突然変異
(defun update-world ()
  "この関数を入力した値分くりかえし、近似を求める"

  (map-salesmans set-distance *salesmans-list* *edge-alist*)
  
  (setf *min-distance-num* (min *min-distance-num* (minimum-distance *salesmans-list*)))
  
  (map-salesmans set-fitness *salesmans-list*  *min-distance-num*)
  
  (let ((c-salesmans (copy-seq *salesmans-list*)))
    (map-salesmans set-probability *salesmans-list* c-salesmans))) 

(defun repl ())
