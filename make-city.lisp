;; make city-node

(defun nodes->alist (nodes)
  (mapcar #'(lambda (x)
              (list x))
          nodes))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop for i from 1 to *city-number*
                     collect (apply #'append (loop for j from 1 to *city-number*
                                                collect (edge-pair i j))))))

(defun direct-edges (node edge-list)
  (remove-if-not #'(lambda (x)
                     (eql (car x) node))
                 edge-list))

;; city-node -> city-edge

(defun edges-to-alist (edge-list)
  (mapcar
   #'(lambda (node1)
       (cons node1
             (mapcar #'(lambda (edge)
                         (list (cdr edge)))
                     (remove-duplicates (direct-edges node1 edge-list)
                                        :test #'equal))))
   (remove-duplicates (mapcar #'car edge-list))))

(defun get-distance (start end edge-alist)
  "始点終点間の距離"
  (car (cdr (find end (cdr (assoc start edge-alist)) :key #'car))))

(defun add-distance (edge-alist)
  "終点が始点より数字が小さい場合(ex. node1 = 3, node2 = 1)、始点終点どちらから辿っても同じになるようにする処理のため、nilにしておく"
  (mapcar
   #'(lambda (x)
       (let ((node1 (car x))
             (node1-edge (cdr x)))
         (cons node1
               (mapcar #'(lambda (edge)
                           (let ((node2 (car edge)))
                             (list node2 (if (< node1 node2)
                                             (1+ (random *max-distance-num*))))))
                       node1-edge))))
   edge-alist))

(defun complete-city-edge (edge-alist)
  (mapcar
   #'(lambda (x)
       (let ((node1 (car x))
             (node1-edge (cdr x)))
         (cons node1
               (mapcar #'(lambda (edge)
                           (let ((node2 (car edge))
                                 (dist (car (cdr edge))))
                             (list node2 (if dist
                                             dist
                                             (get-distance node2 node1 edge-alist))))) ;すでに求めてある値を利用する
                       node1-edge))))
   edge-alist))

(defun make-city-edges ()
  (complete-city-edge (add-distance (edges-to-alist (make-edge-list)))))

;; draw-city

(defun draw-city (edge-alist)
  (let ((node (loop for i below *city-number* collect (1+ i))))
    (ugraph->png "city.dat" (nodes->alist node) edge-alist)))
