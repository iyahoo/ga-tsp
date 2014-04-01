(load "graph-util")
(defconstant +city-number+ 10)
(defconstant +edge-num+ 50)
(defconstant +max-distance-num+ 10)

(defstruct human genes fitness distance)

(defparameter *edge-list* (make-edge-list))
(defparameter *edge-alist* (make-city-edges))
(defparameter *nodes* (loop for i below +city-number+ collect (1+ i)))

;;(make-human :GENES (gene-code) :fitness 100 :distance 0)

(defun gene-code ()
  "重複のないランダムな1-10の遺伝子をつくる"
  (let ((genes))
    (loop for i
       while (< (length genes) 10)	 
       do (let ((num (1+ (random 10))))
            (unless (member num genes)
              (push num genes))))
    genes))

(defun not-random-gene-code ()
  (let ((genes)
        (*random-state* (make-random-state nil)))
    (loop for i
       while (< (length genes) 10)	 
       do (let ((num (1+ (random 10))))
            (unless (member num genes)
              (push num genes))))
    genes))

;; make city-node

(defun random-node ()
  (1+ (random +city-number+)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat +edge-num+
                     collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not #'(lambda (x)
                     (eql (car x) node))
                 edge-list))

(defun get-conneced (node edge-list)
  (let ((visited))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc #'(lambda (edge)
                           (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  "孤立したノードを探索"
  (let ((islands))
    (labels ((find-island (nodes)
               (let* ((connected (get-conneced (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caddr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  "全てのノードをつなぐ"
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; change city-node -> city-edge

(defun make-city-edges ()
  (let* ((nodes *nodes*)
         (edge-list (connect-all-islands nodes *edge-list*)))
    (add-distance (edges-to-alist edge-list))))

(defun edges-to-alist (edge-list)
  (mapcar
   #'(lambda (node1)
       (cons node1
             (mapcar #'(lambda (edge)
                         (list (cdr edge)))
                     (remove-duplicates (direct-edges node1 edge-list)
                                        :test #'equal))))
   (remove-duplicates (mapcar #'car edge-list))))

(defun add-distance (edge-alist)
  (mapcar
   #'(lambda (x)
       (let ((node1 (car x))
             (node1-edge (cdr x)))
         (cons node1
               (mapcar #'(lambda (edge)
                           (let ((node2 (car edge)))
                             (list node2 (random +max-distance-num+))))
                       node1-edge))))
   edge-alist))

(defun node->alist (nodes)
  (mapcar #'(lambda (x)
              (list x))
          nodes))

;; draw-city

(defun draw-city ()
  (ugraph->png "city.dat" (node->alist *nodes*) *edge-alist*))


