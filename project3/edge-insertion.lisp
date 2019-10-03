
(load "../helpers.lisp") ;; yuck I should really do pacakges

(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :edge-insertion
  (:use :common-lisp :iterate :alexandria))
(in-package :edge-insertion)


;; procedure: pick 1 random point
;; select nearest 2 points
;; for all 4 edges, check distance to every point not in the path
;; insert that point

;; Also I want to have animation for all this
(defun sort-by-distances-from-point (p1 points)
  "returns points sorted with closest point to p1 first."
  (mapcar
   #'cdr
   (sort (iter (for p2 in points)
	       (collect (cons (helpers:distance p1 p2) p2)))
	 #'< :key #'car)))

(defun interpolate-between-points (p1 p2 s)
  "for s <= 0, gets p1, s>=1 gets p2, everyhting in between is
    linear iterpolation"
  (cond ((>= s 1) p2)
	((<= s 0) p1)
	(t
	 (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list p1 p2)
	   (declare (ignore n1 n2))
	   (list
	    -1
	    (+ x1 (* (- x2 x1) s))
	    (+ y1 (* (- y2 y1) s)))))))

(defun closest-point-on-line (p0 p1 p2)
  "gets point on line segment p1-p2 closest to p0"
  (destructuring-bind ((n0 x0 y0) (n1 x1 y1) (n2 x2 y2)) (list p0 p1 p2)
    (declare (ignore n0 n1 n2))
    (let* ((x21 (- x2 x1))
	   (y21 (- y2 y1))
	   (x01 (- x0 x1))
	   (y01 (- y0 y1)))
      (let ((s-between-points
	     (/ (+ (* x01 x21) (* y01 y21))
		(+ (* x21 x21) (* y21 y21)))))
	(interpolate-between-points
	  p1 p2 s-between-points)))))

(defun distance-to-line-segment (p0 p1 p2)
  (helpers:distance p0 (closest-point-on-line p0 p1 p2)))

(defun insert-before-el (old-el new-el ls)
  "if insert before first el, inserts at end!
   old-el must be in list."
  (iter (for rest initially ls then (cdr rest))
	(for p-rest previous rest initially (last ls))
	(when (equal (car rest) old-el)
	  (setf (cdr p-rest) (cons new-el (cdr p-rest)))
	  (finish)))
  ls)

(defun insert-after-el (old-el new-el ls)
  "old-el must be in list"
  (iter (for rest initially ls then (cdr rest))
	(when (equal (car rest) old-el)
	  (setf (cdr rest) (cons new-el (cdr rest)))
	  (finish)))
  ls)

(defun insert-at-edge (p0 p1 p2 ls)
  " finds edge with p1 and p2, and puts p0 there"
  (iter (for pn in ls)
	(for pm previous pn initially (last-elt ls))
	(when (and (equal p1 pn) (equal p2 pm))
	  (leave (insert-before-el p1 p0 ls)))
	(when (and (equal p2 pn) (equal p1 pm))
	  (leave (insert-before-el p2 p0 ls)))
	(finally (error "couldnt find edge"))))

;;(apropos "INFINITY")
;; TODO make not system specific
(defparameter ∞ sb-ext:single-float-positive-infinity)

(defun cost (cycle)
  "find cost of a cycle"
  (iter (for p1 in cycle)
	(for p2 previous p1 initially (last-elt cycle))
	(summing (helpers:distance p1 p2))))

;; seed random number generator, only run once.
;;(with-open-file (s "seed.sexp" :direction :output :if-exists :supersede) 
;;  (format s "~A" (make-random-state t)))

(defun min-cost-insertion-edge (p0 cycle)
  "finds the edge to insert p0 at where you get the lowest cost"
  (iter (for p1 in cycle)
	(for p2 previous p1 initially (last-elt cycle))
	(finding (list p1 p2) minimizing (- (+ (helpers:distance p0 p1)
					       (helpers:distance p0 p2))
					    (helpers:distance p1 p2)))))

(defun open-dat-file (is-random n-points &key (if-exists :append))
  "opens file for appending based on our naming scheme"
  (open (format nil "report/plotdata/~a-~a.dat"
		(if is-random "rand" "near")
		n-points)
	:direction :output :if-exists if-exists))

(defun clear-dat-file (is-random n-points)
  "clears the dat files used to store result costs"
  (close (open-dat-file is-random n-points :if-exists :supersede)))
  
  
(defun solve (filename n-points starting-point random-restarts)
  "solve a tsp with a given starting configuration
    This does a lot of file i-o for creating graphical
    artifacts, so its really not that fast. But it doesn't
    need to be!"
  (let* ((g (open 
	     (format nil "outfiles/~a-~A-~A.sexp" (if random-restarts "rand" "near")
		     n-points starting-point)
	     :direction :output :if-exists :supersede))
	 (points (cadr (helpers:get-points filename)))
	 (results-dat-file (open-dat-file random-restarts n-points))
	 (sorted (if random-restarts
		     (shuffle points)
		     (sort-by-distances-from-point (nth starting-point points) points)))
	 (cycle (subseq sorted 0 3))
	 (rest (subseq sorted 3)))
    (iter (format g "(points ~a)~%" rest)
	  (format g "(cycle ~a)~%" cycle)
	  (when (not rest)
	    (format g "END~%")
	    (format results-dat-file "~A~%" (cost cycle))
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
	    (let ((p-closest (closest-point-on-line p0 p1 p2)))
	      (format g "(best-best-arrow ~a ~a)~%" p0 p-closest)
	      (format g "END~%")
	      (destructuring-bind (edge-p1 edge-p2) (min-cost-insertion-edge p0 cycle)
		(setf cycle (insert-at-edge p0 edge-p1 edge-p2 cycle)))
	      (setf rest (delete p0 rest :test #'equal)))))
    (close results-dat-file)
    (close g)))

;; run it for all conbinations
(progn
  (with-open-file (s "seed.sexp") ;; seed random number genrator
    (setq *random-state* (read s)))
  (iter (for is-random in '(t nil))
	(iter (for filename in '("Random30.tsp" "Random40.tsp"))
	      (for n-points in '(30 40))
	      (clear-dat-file is-random n-points)
	      (iter (for n below n-points)
		    (solve filename n-points n is-random)))))

;; time complexity analysis
;;(time (solve  "Random30.tsp" 30 0 nil))

;; time results
(/ 0.153 0.072)
(defun f (n)
  (* (/ 1 6)
     n
     (+ -4 (* 3 n) (* n n))))

;; theoretical results
(/ (f 40) (f 30))
