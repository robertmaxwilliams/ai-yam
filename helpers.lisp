;; LOAD this file to get these definnitions.
;; I really should make some proper packages but like this is 
;; so easy.
(ql:quickload :cl-interpol) ;; for \t \n syntax in strings
(cl-interpol:enable-interpol-syntax)
(ql:quickload :cl-ppcre) ;; for regex
(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :helpers
  (:use :common-lisp :iterate :alexandria)
  (:export "JOIN-LINES" "FOR-IN-FILE-LINES" "GET-POINTS"
	   "VECTOR-TO-LIST" "COORD-DISTANCE" "DISTANCE"
	   "DRAW-LIST-TREE" "SPLIT-ON-P" "MULTIPLE-FUNCALL"))
      

(in-package :helpers)

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

(defun distance (p1 p2)
  "eucledian distance between two (n x y) points"
  (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list p1 p2)
    (declare (ignore n1 n2))
    (coord-distance x1 x2 y1 y2)))

(defun split-on-p (predicate ls)
  (let ((ls1 nil)
	(ls2 nil))
    (iter (for el in ls)
	  (if (funcall predicate el)
	      (push el ls1)
	      (push el ls2)))
    (values ls1 ls2)))

(defmacro multiple-value-setf (variables form)
  (let ((syms (loop repeat (length variables) collect (gensym))))
    `(multiple-value-bind
      ,syms ,form
      ,@(loop for sym in syms
	   for var in variables
	   collect `(setf ,var ,sym)))))




(defun multiple-funcall (times fun arg)
  "single arg functions only"
  (let ((result arg))
    (iter (repeat times)
	  (setq result (funcall fun result)))
    result))


(defun draw-list-tree (ls &optional (is-last nil) (indent-str "") (is-root t))
  (if is-root (format t "~%"))
  (if (>= (length indent-str) 1)
      (format t "~a" (subseq indent-str 0 (- (length indent-str) 1)))
      (format t "~a" indent-str))
  (when (not is-root)
    (if is-last
	(format t "├")
	(format t "└")))
  (if (not (atom (cadr ls)))
      (progn
	(format t "~a~%" (car ls))
	(iter (for x on (cdr ls))
	      (cond
		((null x)
		 (format t "aslkdslkjdlkasjd~%"))
		((null (cdr x))
		  (draw-list-tree (car x)  nil (concatenate 'string indent-str " ") nil))
		(t
		  (draw-list-tree (car x) t (concatenate 'string indent-str "│") nil)))))
      (format t "~{~a~^=> ~}~%" ls)))


