;; This project has 2 parts - data munging the source files and trying all permutations.
;; Let's munge that data!

;; First part is of the format "NAME: some text"
;; Then the line "NODE_COORD_SECTION"
;; then enumerated line by line like this: "1 87.951292 2.658162"

(ql:quickload :cl-interpol) ;; for \t \n syntax in strings
(cl-interpol:enable-interpol-syntax)
(ql:quickload :local-time) ;; for microsecond time precision
(ql:quickload :cl-ppcre) ;; for regex
(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage brute-force
  (:use :common-lisp :iterate :alexandria))

(in-package :brute-force)

(defun set-optimized (optimize)
  (if optimize
      (unless (member :is-optimized *features*)
	(push :is-optimized *features*))
      (when (member :is-optimized *features*)
	(setq *features* (remove :is-optimized *features*)))))

(set-optimized t)
;;(set-optimized nil)

(progn
  #+is-optimized (format t "optimized!~%")
  #+(not is-optimized) (format t "not optimized~%"))

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
  #+is-optimized (declare (optimize (speed 3) (safety 0)))
  #+is-optimized (declare (single-float x1 x2 y1 y2))
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(deftype point-triplet ()
  `(cons fixnum
	 (cons single-float
	       (cons single-float null))))

(defun distance (p1 p2)
  "eucledian distance between two (n x y) points"
  #+is-optimized (declare (optimize (speed 3) (safety 0)))
  #+is-optimized (declare (point-triplet p1 p2))
  (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list p1 p2)
    (declare (ignore n1 n2))
    #+is-optimized (declare (single-float x1 y1 x2 y2))
    (coord-distance x1 x2 y1 y2)))

(defun evaluate (seq)
  "get distance between each pair of points"
  #+is-optimized (declare ((vector point-triplet) seq))
  (iter (for p1 in-vector seq)
	(for p2 previous p1 initially (last-elt seq))
	(sum (distance p1 p2))))
 
(defun find-best-path-helper (seq &optional (n 1)) ;; start at n=1 so first element isnt permuted
  " create all permutations of a seq, collect min cost sequence.
  returns (loss, seq) pair"
  #+is-optimized (declare (optimize (speed 3) (safety 0)))
  #+is-optimized (declare ((vector point-triplet) seq))
  (labels ((swap-recur-unswap (seq a b)
	     #+is-optimized (declare ((vector point-triplet) seq)
				     (fixnum a b))
	     (progn
	       (rotatef (aref seq a) (aref seq b))
	       (let ((result 
		      (find-best-path-helper seq (1+ n))))
		 (rotatef (aref seq a) (aref seq b))
		 result))))
    (let ((len (length seq)))
      #+is-optimized (declare (fixnum len))
      (cond ((= n (1- len))
	     (list (evaluate seq) (copy-seq seq)))
	    (t
	     (let ((loss-and-seq nil)
		   (min-loss 100000.0)
		   (best-loss-and-seq nil))
	       #+is-optimized
	       (declare ((or null (cons single-float
					(cons (vector point-triplet)
					      null)))
			 loss-and-seq best-loss-and-seq)
			(single-float min-loss))
	       (loop for i from n below len
		  do (setq loss-and-seq (swap-recur-unswap seq i n))
		  when (or (null min-loss) (< (car loss-and-seq) min-loss))
		  do (setq best-loss-and-seq loss-and-seq)
		  and do (setq min-loss (car loss-and-seq)))
	       best-loss-and-seq))))))
		   ;;(finding loss-and-seq minimizing (+ (car loss-and-seq) (car (aref seq 1))))))))))

(defun find-best-path (filename)
  "uses a helper to seperate loading from recursion"
  (find-best-path-helper (apply #'vector (cadr (get-points filename)))))

  
(let ((time-millis nil)
      (time-micros nil))
  (defun timer-start ()
    "starts a timer. Read out elapsed time by calling timer-end."
    (setq time-millis (get-internal-real-time))
    (setq time-micros (local-time:timestamp-microsecond (local-time:now)))
    nil)
  (defun timer-end ()
    "time since last call to timer-start. 
    Returns pair (n units) with units of secs, millis or micros."
    (let* ((dt-micros (- (local-time:timestamp-microsecond (local-time:now)) time-micros))
	   (dt-millis (- (get-internal-real-time) time-millis)))
      (cond
	((> dt-millis 1000)
	 (list (* 1e-3 dt-millis) 'secs))
	((> dt-millis 1)
	 (list dt-millis 'millis))
	(t 
	  (list (floor (mod dt-micros 1e6)) 'micros))))))
  
(defun print-all-solutions (&optional (max-n 9))
  "creates nice printout with one solution per line"
  (format t "~%~%~%")
  (format t #?"~%name    \ttime    \tdistance\tpath~%")
  (iter (for n from 4 to max-n)
	(timer-start)
	(let* ((distance-path (find-best-path (format nil "Random~a.tsp" n)))
	       (distance (car distance-path))
	       (path (vector-to-list (cadr distance-path)))
	       (time (timer-end))
	       (path-ns (mapcar #'car path)))
	  (format t #?"Random~a.tsp\t~{~a~^ ~}\t~a\t[~{~a~^, ~}]~%" n time (round distance) path-ns))))

(defun solve-and-write-to-file (n)
  "call with n to open tsp, solve it, and write a solution tsp out"
  (with-open-file (outfile (format nil "Random~a_solution.tsp" n) :direction :output :if-exists :supersede)
    (let* ((start-time (get-internal-real-time))
	   (distance-path (find-best-path (format nil "Random~a.tsp" n)))
	   (distance (car distance-path))
	   (path (vector-to-list (cadr distance-path)))
	   (time (/ (- (get-internal-real-time) start-time) 1000.0)))
      (format
       outfile
       (join-lines
	"NAME: concorde~a"
	"TYPE: TSP"
	"RUN_TIME: ~a"
	"DISTANCE: ~a"
	"COMMENT: Solution by max williams"
	"DIMENSION: ~a"
	"EDGE_WEIGHT_TYPE: EUC_2D"
	"NODE_COORD_SECTION")
       n time (round distance) n) 
      (iter (for (nn x y) in path)
	    (format outfile "~a ~,6f ~,6f~%" nn x y)))))

(defun solve-all-and-write-to-files ()
  (iter (for n from 4 to 12)
	(solve-and-write-to-file n)
	(format t "solved ~a~%" n)))

(print-all-solutions 10)
;;(solve-all-and-write-to-files)
