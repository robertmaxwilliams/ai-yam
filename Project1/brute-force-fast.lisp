(ql:quickload :cl-interpol) ;; for \t \n syntax in strings
(cl-interpol:enable-interpol-syntax)
(ql:quickload :local-time) ;; for microsecond time precision
(ql:quickload :cl-ppcre) ;; for regex
(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage brute-force-fast
  (:use :common-lisp :iterate :alexandria))

(in-package :brute-force-fast)

(defun join-lines (&rest strings)
  "joins lines with newline, and ends with newline"
  (format nil "~{~a~%~}" strings))

(defmacro for-in-file-lines ((var-name file-name) &body body)
  "iterate through lines in a file"
  (let ((in (gensym)))
    `(let ((,in (open ,file-name :if-does-not-exist nil)))
       (when ,in
	 (loop for ,var-name = (read-line ,in nil)
	    while ,var-name do (progn ,@body)))
	 (close ,in))))

(defun get-points (filename)
  "takes string filename and returns (metadata points) pair.
   Points are list of (n x y) triples"
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
    (list header (reverse points))))

(defun vector-to-list (vec)
  (iter (for x in-vector vec)
	(collect x)))

(defun coord-distance (x1 x2 y1 y2)
  "mathy distance function"
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defvar *inf* sb-ext:single-float-positive-infinity)

(defun build-distance-matrix (points)
  (let* ((n (length points))
	 (distance-matrix
	  (make-array (list (1+ n) (1+ n))
		      :element-type 'single-float
		      :initial-element 0.0)))
    (iter (for (n1 x1 y1) in points)
	  (iter (for (n2 x2 y2) in points)
		(setf (aref distance-matrix n1 n2) (coord-distance x1 x2 y1 y2))))
    distance-matrix))

(deftype point-sequence ()
  `(simple-array fixnum *))

(deftype distance-matrix ()
  `(simple-array single-float (* *)))

(defmacro evaluate-definer (n)
  (format t "defining evaluator for n = ~a~%" n)
  `(defun evaluate (seq distance-matrix)
     (declare (point-sequence seq)
	      (distance-matrix distance-matrix))
     (+ ,@(loop for i from 0 below n
	     and j = (1- n) then i
	     collect `(aref distance-matrix (aref seq ,j) (aref seq ,i))))))


;; gets tricky here

(define-symbol-macro +n-points+ 4)

(disassemble #'evaluate)

(progn
  (defun recur-permutations (seq k max-k distance-matrix)
    ;;(declare (optimize (speed 3) (safety 0)))
    (declare (point-sequence seq)
	     (distance-matrix distance-matrix)
	     (fixnum k max-k))
    (if (= k max-k)
	(let ((cost (evaluate seq distance-matrix)))
	  (declare (single-float cost))
	  (when (< cost min-cost)
	    (setq min-cost cost)
	    (setq best-seq (copy-seq seq))))
	(loop for i from k below max-k
	   do (rotatef (aref seq i) (aref seq k))
	   do (recur-permutations seq (1+ k) max-k distance-matrix)
	   do (rotatef (aref seq i) (aref seq k)))))
  (evaluate-definer #.+n-points+)

  (defparameter min-cost *inf*)
  (defparameter best-seq (make-array 1 :element-type 'fixnum :initial-element -1))
  (proclaim
   '(type single-float min-cost))
  (proclaim
   '(type point-sequence best-seq))
  (proclaim '(optimize (speed 3) (safety 0) (debug 0)))

  (let* ((points (cadr (get-points (format nil "Random~a.tsp" +n-points+))))
	 (distance-matrix (build-distance-matrix points))
	 (points-numbers (make-array (length points)
				     :element-type 'fixnum
				     :initial-contents (mapcar #'car points))))
    (time (recur-permutations points-numbers 1 (length points) distance-matrix))
    (format t "~A~%" (list best-seq min-cost))))
 
