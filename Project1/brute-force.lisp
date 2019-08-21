;; This project has 2 parts - data munging the source files and trying all permutations.
;; Let's munge that data!

;; First part is of the format "NAME: some text"
;; Then the line "NODE_COORD_SECTION"
;; then enumerated line by line like this: "1 87.951292 2.658162"

(ql:quickload "cl-ppcre")
(ql:quickload :iterate)
(ql:quickload :alexandria)
(defpackage brute-force
  (:use :common-lisp :iterate :alexandria))

(in-package :brute-force)


(defmacro for-in-file-lines ((var-name file-name) &body body)
  (let ((in (gensym)))
    `(let ((,in (open ,file-name :if-does-not-exist nil)))
       (when ,in
	 (loop for ,var-name = (read-line ,in nil)
	    while ,var-name do (progn ,@body)))
	 (close ,in))))

(defun get-points (filename)
  (let ((is-header t)
	(header nil)
	(points nil))
    (for-in-file-lines
	(line filename)
      (cond ((ppcre:scan "NODE_COORD_SECTION" line)
	     (setq is-header nil))
	    (is-header
	     (ppcre:register-groups-bind (key value)
		 ("(\\w+): (\\w+)" line)
	       (push (list key value) header)))
	    (t
	     (ppcre:register-groups-bind (num x y)
		 ("(\\d+) ([,.\\d]+) ([,.\\d]+)" line)
	       (push (mapcar #'read-from-string (list num x y)) points)))))
    (list header points)))

;; get into the REAL programming
(defun seq-permutations (seq &optional (n 0))
  " create all permutations of a seq, return list of sequences"
  (let ((len (length seq)))
    (cond ((= n (1- len))
	   (list (copy-seq seq)))
	  (t
	   (loop for i from n below len
	      append (progn
			(rotatef (aref seq i) (aref seq n))
			(let ((result 
			       (seq-permutations seq (1+ n))))
			  (rotatef (aref seq i) (aref seq n))
			  result)))))))

(defun distance (point-1 point-2)
  " euclidean distance, based on (id x y) format of points"
  (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list point-1 point-2)
    (declare (ignore n1 n2))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))


(defun evaluate (seq-of-points)
  " gives distance of some traversal"
  (iter (for point-2 in-vector seq-of-points)
	   (for point-1 previous point-2)
	   (if-first-time (next-iteration))
	   (sum (distance point-1 point-2))))
;;(evaluate #((1 1.2 1.3) (2 0.1 0.2) (2 0.9 0.2)))

(defun vector-to-list (vec)
  (iter (for x in-vector vec)
	(collect x)))

(defun find-best-path (filename)
  "big ol' finder function"
  (let* ((header-points (get-points filename))
	 (header (car header-points))
	 (points (cadr header-points))
	 (permutations (seq-permutations (apply #'vector points))))
    (list
     header
     (iter (for list-of-points in permutations)
	   (for cost = (evaluate list-of-points))
	   (finding (list cost (vector-to-list list-of-points)) minimizing cost)))))

(defun coord-distance (x1 x2 y1 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
  
	
(defun memory-efficient-find-best-path-helper (seq &optional (n 0))
  " create all permutations of a seq, collect min cost sequence"
  (let ((distance-matrix
	 (make-array
	  (list (length seq) (length seq))
	  :initial-contents
	  (iter (for (n1 x1 y1) in-vector seq)
		(collect (iter (for (n2 x2 y2) in-vector seq)
			       (collect (coord-distance x1 x2 y1 y2))))))))
    (labels ((distance-fast (p1 p2)
	       (aref distance-matrix (1- (car p1)) (1- (car p2))))
	     (evaluate-fast (seq-of-points)
	       (iter (for point-2 in-vector seq-of-points)
		     (for point-1 previous point-2 initially (last-elt seq-of-points))
		     (sum (distance-fast point-1 point-2))))
	     (swap-recur-unswap (seq a b)
	       (progn
		 (rotatef (aref seq a) (aref seq b))
		 (let ((result 
			(memory-efficient-find-best-path-helper seq (1+ n))))
		   (rotatef (aref seq a) (aref seq b))
		   result))))
      (let ((len (length seq)))
	(cond ((= n (1- len))
	       (list (evaluate seq) (copy-seq seq)))
	      (t
	       (iter (for i from n below len)
		     (for loss-and-seq = (swap-recur-unswap seq i n))
		     (finding loss-and-seq minimizing (car loss-and-seq)))))))))

(defun memory-efficient-find-best-path (filename)
  (memory-efficient-find-best-path-helper (apply #'vector (cadr (get-points filename)))))

(format t "~a~%~%" (find-best-path "Random4.tsp"))
  
(format t "~a~%~%" (memory-efficient-find-best-path "Random4.tsp"))
;; (134.0372
;;  #((4 20.526749 47.63329) (2 33.4666 66.682945) (3 91.77831 53.807182)
;;    (1 87.951294 2.658162)))

(format t "~a~%~%" (memory-efficient-find-best-path "Random5.tsp"))
;; (92.57153
;;  #((4 50.735294 33.333332) (3 64.70588 76.16034) (5 53.104576 90.7173)
;;    (2 42.565357 77.00422) (1 31.045752 75.52743)))

(format t "~a~%~%" (memory-efficient-find-best-path "Random6.tsp"))
;; (89.410904
;;  #((4 60.04902 48.734177) (5 43.545753 49.156116) (6 30.228758 51.47679)
;;    (1 30.147058 79.74683) (2 45.99673 78.27004) (3 61.19281 78.27004)))

(format t "~a~%~%" (memory-efficient-find-best-path "Random7.tsp"))
;; (44.848824
;;  #((7 27.042484 59.07173) (3 21.486929 67.08861) (2 29.003267 69.83122)
;;    (6 25.571896 72.57384) (5 24.019608 78.691986) (1 29.656862 81.22363)
;;    (4 20.915033 86.49789)))

(time (format t "~a~%~%" (memory-efficient-find-best-path "Random8.tsp")))
;; (238.81973
;;  #((5 9.006012 81.18534) (2 33.4666 66.682945) (4 20.526749 47.63329)
;;    (8 41.059605 32.57851) (6 20.03235 2.761925) (1 87.951294 2.658162)
;;    (7 77.18131 31.922361) (3 91.77831 53.807182)))

(time (format t "~a~%~%" (memory-efficient-find-best-path "Random9.tsp")))
;; (105.01195
;;  #((5 52.108433 47.905758) (3 41.566265 37.303665) (6 33.373493 45.81152)
;;    (7 30.481928 55.497383) (1 21.024096 75.65445) (8 38.192772 78.01047)
;;    (4 37.048195 74.34555) (9 42.168674 67.93194) (2 57.771084 73.29843)))

(format t "~a~%~%" (memory-efficient-find-best-path "Random10.tsp"))
;; (84.03515
;;  #((6 23.77451 59.70464) (7 25.245098 67.72152) (8 30.06536 66.24473)
;;    (5 38.071896 60.759495) (9 36.02941 70.88608) (10 49.264706 71.940926)
;;    (4 40.27778 80.379745) (3 30.392157 79.3249) (2 23.039215 81.4346)
;;    (1 22.54902 89.02953)))

(format t "~a~%~%" (memory-efficient-find-best-path "Random11.tsp"))
;; (84.03515
;;  #((6 23.77451 59.70464) (7 25.245098 67.72152) (8 30.06536 66.24473)
;;    (5 38.071896 60.759495) (9 36.02941 70.88608) (10 49.264706 71.940926)
;;    (4 40.27778 80.379745) (3 30.392157 79.3249) (2 23.039215 81.4346)
;;    (1 22.54902 89.02953)))

(format t "~a~%~%" (memory-efficient-find-best-path "Random12.tsp"))
;; (56.813557
;;  #((12 29.820261 65.822784) (4 35.702614 59.2827) (9 31.862745 55.27426)
;;    (5 29.084967 52.109707) (10 25.571896 53.16456) (6 21.650328 58.016876)
;;    (7 22.46732 64.55696) (11 24.183006 69.19831) (1 25.816994 74.261604)
;;    (8 28.676472 76.16034) (2 32.35294 77.42616) (3 34.477123 73.83966)))



