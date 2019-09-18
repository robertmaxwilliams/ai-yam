(load "../helpers.lisp") ;; yuck I should really do pacakges

(ql:quickload :cl-interpol) ;; for \t \n syntax in strings
(cl-interpol:enable-interpol-syntax)
(ql:quickload :cl-ppcre) ;; for regex
(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :tree-search
  (:use :common-lisp :iterate :alexandria))
(in-package :tree-search)

(defparameter connections
  '((1 2 3 4) ;; means "1 can go to 2, 3, or 4"
    (2 3)
    (3 4 5)
    (4 5 6 7)
    (5 7 8)
    (6 8)
    (7 9 10)
    (8 9 10 11)
    (9 11)
    (10 11)
    (11 :end)))

(defun next-points (p points)
  " helper to get next point using alist"
  (iter (for p2-key in (cdr (assoc (car p) connections)))
	(collect (assoc p2-key points))))

(defun dfs (points)
  " get solution using dfs"
  (let ((lowest-cost nil)
	(best-path nil))
    (labels
	((dfs-recur (current-point path cost)
	   (cond
	     ((= (car current-point) 11)
	      (when (or (null lowest-cost) (< cost lowest-cost))
		(setq lowest-cost cost)
		(setq best-path path)))
	     (t
	      (iter (for p in (next-points current-point points))
		    (dfs-recur p
			       (cons (car p) path)
			       (+ cost (helpers:distance p current-point))))))))
      (dfs-recur (assoc 1 points) (list 1) 0)
      (list lowest-cost (reverse best-path)))))

;; fyi path is list of (cost (p4 p3 p2 p1)) or such
(defun expand-path (cost+path points)
  " expands path into successive paths"
  (destructuring-bind (cost path) cost+path
    (iter (for next-point in (next-points (car path) points))
	  (if (null next-point) (next-iteration))
	  (for cost-to-next-point = (helpers:distance next-point (car path)))
	  (collect (list (+ cost cost-to-next-point) (cons next-point path))))))

(defun expand-paths (cost+paths points)
  "takes a list of paths and creates all successive paths"
  (iter (for cost+path in cost+paths)
	(appending (expand-path cost+path points))))

(defun bfs (points max-depth)
  " expands paths at each step, moves to complete paths if it finds 11"
  (let ((paths (list (list 0 (assoc 1 points))))
	(complete-paths nil))
    (iter (repeat max-depth) 
	  (multiple-value-bind (new-solutions next-paths)
	      (expand-paths paths points)
	    (setq complete-paths (append new-solutions complete-paths))
	    (setq paths next-paths)))
    (list complete-paths paths)))
  

(defun dfs-tree (points &optional current-point (cost 0))
  (when (not current-point)
    (setq current-point (assoc 1 points)))
  (cond
    ((= (car current-point) 11)
     (list (car current-point) cost ))
    (t
     (let* ((next-points-keys (cdr (assoc (car current-point) connections)))
	    (next-points (iter (for p in points)
			       (when (member (car p) next-points-keys)
				 (collect p)))))
       (list next-points-keys next-points cost)
       (cons
	(car current-point)
	(iter
	  (for p in next-points)
	  (collect
	      (dfs-tree points p (+ cost (helpers:distance p current-point))))))))))

(format t "BFS each step printout: ~%")

(let* ((points (cadr (helpers:get-points "11PointDFSBFS.tsp")))
       (result (list (list 0 (list (assoc 1 points))))))
  (iter (for i from 0)
	(if (null result) (return))
	(format t "Step: ~a~% Paths: ~%~{  ~a~%~}~%~%" i
		(sort (mapcar
		       (lambda (foo)
			 (list (car foo)
			       (mapcar #'car (cadr foo))))
		       result)
		      (lambda (a b) (if (= (caadr a) (caadr b))
					(< (car a) (car b))
					(> (caadr a) (caadr b))))))
	(setq result (expand-paths result points)))
  result)

(format t "DFS s-exp and solution: ~%")

(let ((points (cadr (helpers:get-points "11PointDFSBFS.tsp"))))
  (format t "~A~%~%" (dfs-tree points))
  (format t "solution: ~A~%~%" (dfs points)))

(format t "Pretty tree for DFS: ~%")

(let ((points (cadr (helpers:get-points "11PointDFSBFS.tsp"))))
  (helpers:draw-list-tree (dfs-tree points)))
