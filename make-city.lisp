;; make city-node

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

;; change city-node -> city-edge

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
                             (list node2 (1+ (random *max-distance-num*)))))
                       node1-edge))))
   edge-alist))

(defun make-city-edges ()
  (add-distance (edges-to-alist (make-edge-list))))


(defparameter *edge-alist* (make-city-edges))

(defun nodes->alist (nodes)
  (mapcar #'(lambda (x)
              (list x))
          nodes))

;; draw-city

(defun draw-city ()
  "*edge-alist* -> (make-city-edges)"
  (let ((node (loop for i below *city-number* collect (1+ i))))
    (ugraph->png "city.dat" (nodes->alist node) *edge-alist*)))
