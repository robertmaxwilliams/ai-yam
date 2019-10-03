;; wumpus game: get the gold, kill the wumpus. There is one wumpus and three pits, in 4x4 grid
;; actions: rotate l/r, move forward, shoot arrow, pick up gold
;; sense: glitter, smelly (near wumpus), breezy (near pit)
(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :wumpus
  (:use :common-lisp :iterate :alexandria))
(in-package :wumpus)

(defun random-pair (max existing-pairs-list)
  (let ((p (list (random max) (random max))))
    (if (member p existing-pairs-list :test #'equal)
	(random-pair max existing-pairs-list)
	p)))

(defun create-n-unique-pairs (n max &optional (forbidden nil))
  (let ((pairs nil))
    (loop repeat n
       do (push (random-pair max (append forbidden pairs)) pairs))
    pairs))
(create-n-unique-pairs 4 4 '((0 0)))

(defun create-world ()
  (let ((world (make-array '(4 4) :initial-element 'nil)))
    (iter (for (x y) in (create-n-unique-pairs 5 4))
	  (for name in '(pit pit pit wumpus gold))
	  (setf (aref world x y) name))
    world))

(defun get-adjacents (world x y)
  (iter (for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1)))
	(collect (ignore-errors (aref world (+ x dx) (+ y dy))))))

(defun get-info (world x y)
  (remove
   nil
   (list
    (case (aref world x y) 
      (gold 'glitter)
      (pit 'falling)
      (wumpus 'murdered))
    (when (member 'pit (get-adjacents world x y))
      'breezey)
    (when (member 'wumpus (get-adjacents world x y))
      'smelly))))


(defparameter *world* (create-world))
(format t "~A~%" *world*)
(format t "[~{~a~^, ~}]~%" '(a b c d))
(get-info *world* 3 0)
(defun print-world (world &optional player-pos)
  (format t "========~%")
  (iter (for i from 0 below 4)
	(iter (for j from 0 below 4)
	      (let ((thing
		     (case (aref world i j)
		       (gold "G")
		       (wumpus "W")
		       (pit "P")
		       (otherwise nil)))
		    (is-player
		     (and player-pos
			  (= i (car player-pos))
			  (= j (cadr player-pos)))))
		(cond ((and thing is-player)
		       (format t "~a-" thing))
		      (thing (format t "~a " thing))
		      (is-player (format t "- "))
		      (t (format t "  ")))))
	(format t "~%"))
  (format t "========~%"))

(print-world *world* '(0 3))
