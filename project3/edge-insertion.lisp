
(load "../helpers.lisp") ;; yuck I should really do pacakges

(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :edge-insertion
  (:use :common-lisp :iterate :alexandria))
(in-package :edge-insertion)


;; procedure: pick 1 random point
;; select nearest 4 points
;; solve 4 point tsp by brute forcce
;; for all 4 edges, check distance to every point not in the path
;; insert that point

(defun sqr (x)
  (* x x))
;; Also I want to have animation for all this
(defun sort-by-distances-from-point (p1 points)
  (mapcar
   #'cdr
   (sort (iter (for p2 in points)
	       (collect (cons (helpers:distance p1 p2) p2)))
	 #'< :key #'car)))

(defun constrain (x lower upper)
  (cond
    ((< x lower) lower)
    ((> x upper) upper)
    (t x)))

(defun interpolate-between-points (p1 p2 s)
  (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list p1 p2)
    (declare (ignore n1 n2))
    (list
     -1
     (+ x1 (* (- x2 x1) s))
     (+ y1 (* (- y2 y1) s)))))

(defun closest-point-on-line (p0 p1 p2)
  (destructuring-bind ((n0 x0 y0) (n1 x1 y1) (n2 x2 y2)) (list p0 p1 p2)
    (declare (ignore n0 n1 n2))
    (let* ((x21 (- x2 x1))
	   (y21 (- y2 y1))
	   (x01 (- x0 x1))
	   (y01 (- y0 y1)))
      (interpolate-between-points
       p1 p2
       (constrain
	(/ (+ (* x01 x21) (* y01 y21))
	   (+ (* x21 x21) (* y21 y21)))
	0.0 1.0)))))

(defun distance-to-line-segment (p0 p1 p2)
  (helpers:distance p0 (closest-point-on-line p0 p1 p2)))

(defparameter *s* nil)
(iter
  (for n from 0 to 10)
  (let ((*s* 
	 (open 
	  (concatenate 'string "outfiles/" (format nil "~3,'0D" n) ".sexp")
	  :direction :output :if-exists :supersede)))
    (let ((p0 `(0 6 ,n))
	  (p1 '(1 2 5))
	  (p2 '(2 1.1 2.2)))
      (format *s* "(points ~s ~a ~a)~%" p0 p1 p2)
      (format t "~a: ~a~%" n (distance-to-line-segment p0 p1 p2)))
    (close *s*)))


(defun insert-after-el (old-el new-el ls)
  (iter (for rest initially ls then (cdr rest))
	(when (equal (car rest) old-el)
	  (setf (cdr rest) (cons new-el (cdr rest)))
	  (finish)))
  ls)

;;(apropos "INFINITY")
(defparameter ∞ sb-ext:single-float-positive-infinity)


;; version that creates outfile
(defun solve (filename n-points starting-point)
  (let* ((g (open 
	     (format nil "outfiles/foo-~A-~A.sexp" n-points starting-point)
	     :direction :output :if-exists :supersede))
	 (points (cadr (helpers:get-points filename)))
	 (sorted (sort-by-distances-from-point (nth starting-point points) points))
	 (cycle (subseq sorted 0 3))
	 (rest (subseq sorted 3)))
    (iter (format g "(points ~a)~%" rest)
	  (format g "(cycle ~a)~%" cycle)
	  (when (not rest)
	    (format g "END~%")
	    (finish))
	  (for best-edge-point = nil)
	  (for min-distance = ∞)
	  (iter (for p0 in rest)
		(for (p1 p2) =
		     (iter (for p1 in cycle)
			   (for p2 previous p1 initially (last-elt cycle))
			   (format g "(arrow ~a ~a)~%"
				   p0 (closest-point-on-line p0 p1 p2))
			   (let ((d (distance-to-line-segment p0 p1 p2)))
			     (finding (list p1 p2) minimizing d)
			     (when (< d min-distance)
			       (setq best-edge-point (list p0 p1 p2))
			       (setq min-distance d)))))
		(format g "(best-arrow ~a ~a)~%"
			p0 (closest-point-on-line p0 p1 p2)))
	  (destructuring-bind (p0 p1 p2) best-edge-point
	    (format g "(best-best-arrow ~a ~a)~%"
		    p0 (closest-point-on-line p0 p1 p2))
	    (format g "END~%")
	    (setf cycle (insert-after-el p2 p0 cycle))
	    (setf rest (delete p0 rest :test #'equal))))
    (close g)))

(iter (for filename in '("Random30.tsp" "Random40.tsp" ))
      (for n-points in '(30 40))
      (iter (for n below n-points)
	    (solve filename n-points n)))


;; original
(let* ((points (cadr (helpers:get-points "Random30.tsp")))
       (sorted (sort-by-distances-from-point (car points) points))
       (cycle (subseq sorted 0 3))
       (rest (subseq sorted 3)))
  (format t "~%~%")
  (iter (while rest)
	(for best-edge-point = nil)
	(for min-distance =  ∞)
	(iter (for p0 in rest)
	      (collect
		  (list
		   (car p0)
		   (iter (for p1 in cycle)
			 (for p2 previous p1 initially (last-elt cycle))
			 (let ((d (distance-to-line-segment p0 p1 p2)))
			   (when (< d min-distance)
			     (setq best-edge-point (list p0 p1 p2))
			     (setq min-distance d)))))))
	(destructuring-bind (p0 p1 p2) best-edge-point
	  (format t "lowest cost: ~A, new point: ~A, ~%edge: ~A~%"
		  min-distance p0 (list p2 p1))
	  (setf cycle (insert-after-el p2 p0 cycle))
	  (setf rest (delete p0 rest :test #'equal)))
	(format t "~A~%" (list (mapcar #'car cycle) (mapcar #'car rest)))))

(insert-after-el 'a 4 (list 'a 'b 'c 'd 'e))


