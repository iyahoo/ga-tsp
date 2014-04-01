(load "graph-util")

(defconstant +city-number+ 10)
(defconstant +edge-num+ 50)

(defparameter *edge-list* (make-edge-list))
(defparameter *nodes* (loop for i below +city-number+ collect (1+ i)))

(defstruct human genes fitness distance)

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
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

