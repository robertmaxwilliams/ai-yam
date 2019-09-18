;; this is gonna be a ride



;; how to represent rubix cube? I guess six matrices. But how to relate them?
;; The goal here is to create "interpretable" solutions to sub-problems,
;; for making human-useful algorithms for solving cubes. Things like
;; "how to rotate a corner".

;; What operations to we need? Roate face, and the three vertical lines,
;; and the three horizonal lines.

;; colors will be numbers 0-5

(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :rubix
  (:use :common-lisp :iterate :alexandria))
(in-package :rubix)

;; ==========================================================
;; cube array helpers
;; ==========================================================
(defun make-cube ()
  "create fresh numbered cube"
  (let ((cube (make-array '(6 3 3)
			  :element-type '(mod 128)
			  :initial-element 0)))
    (iter (for n from 0 below 6)
	  (iter (for i from 0 below 3)
		(iter (for j from 0 below 3)
		      (setf (aref cube n i j) (+ (* n 10) (* i 3) j)))))
    cube))


(defun print-cube (cube &optional (view #'identity))
  (format t "~%     ====================    ~%")
  (iter (for abc in '((nil 3 nil) (5 0 4) (nil 1 nil) (nil 2 nil)))
	(for row3 from 0)
	(iter (for row from 0 below 3)
	      (iter (for n in abc)
		    (for col3 from 0)
		    (iter (for col from 0 below 3)
			  (if n
			      (format t "~2,'0d "
				      (apply #'aref cube
					     (funcall view (list n row col))))
			      (format t "   ")))
		    (when (< col3 2) (format t " ")))
	      (format t "~%"))
	(format t "~%")))

;; ==========================================================
;; random generic helpers
;; ==========================================================
(defun funcall-n-times (fun times arg)
  "call a function multiple times nested"
  (let ((result arg))
    (iter (repeat times)
	  (setq result (funcall fun result)))
    result))

(defun rotate-ls (ls)
  "rotate the car to the end of list"
  (append (cdr ls) (list (car ls))))

(defun face-rotate-i-j (i j)
  "take i j and return i j of face roated to the right"
  (let* ((ls-i-j '((1 0) (2 0) (2 1) (2 2) (1 2) (0 2) (0 1) (0 0)))
	 (i-j-alist (cons
		     '((1 1) 1 1)
		     (mapcar #'cons ls-i-j (rotate-ls (rotate-ls ls-i-j))))))
    (cdr (assoc (list i j) i-j-alist :test #'equal))))

(defun croncher (ls1 ls2)
  " cross product ls1 and ls2 and iterate"
  (if (atom ls1) (setq ls1 (list ls1)))
  (if (atom ls2) (setq ls2 (list ls2)))
  (iter (for i in ls1)
	(appending
	 (iter (for j in ls2)
		    (collect (list i j))))))

;; ==========================================================
;; VIEW FUNCTIONS 
;; ==========================================================

(defun view-rotate-face-n-maker (n-face)
  "view, rotates nth face(s) right"
  (when (atom n-face) (setq n-face (list n-face)))
  (lambda (c)
    (destructuring-bind (n i j) c
      (if (member n n-face)
	  (cons n (face-rotate-i-j i j))
	  (list n i j)))))

(defun view-faces-column-up (c)
  "view, turns center column upways"
  (destructuring-bind (n i j) c
    (list
     (if (< n 4)
	 (mod (1+ n) 4)
	 n)
       i j)))

(defun cycle-n (c)
  " view, cycle faces in manner needed for spin whole cube right"
  (cons (ecase (car c) (0 0) (1 4) (2 2) (3 5) (4 3) (5 1)) (cdr c)))

(defun view-turn-cube-up (c)
  "view, turn the whole cube up"
  (let ((view-rotate-left-face (view-rotate-face-n-maker 5))
	(view-rotate-right-face (view-rotate-face-n-maker 4)))
    (funcall-n-times view-rotate-left-face 3
		     (funcall view-rotate-right-face
			      (view-faces-column-up c)))))

(defun view-spin-cube-right (c)
  " view, spin while cube right."
  (let ((rotates-main-plus (view-rotate-face-n-maker '(0 1 3 4 5)))
	(rotates-back (view-rotate-face-n-maker 2)))
    (setq c (funcall rotates-main-plus c))
    (setq c (funcall-n-times rotates-back 3 c))
    (setq c (cycle-n c))
    c))
  
;; ==========================================================
;; pairswap generating functions
;; ==========================================================

(defun rotate-face-cw ()
  (append
   (let* ((ls-i-j (reverse '((0 0) (0 1) (0 2) (1 2) (2 2) (2 1) (2 0) (1 0))))
	  (pp-ls-i-j (cddr ls-i-j)))
     (iter (for (i j) in ls-i-j)
	   (for (pp-i pp-j) in pp-ls-i-j)
	   (collect (list (list 0 i j) (list 0 pp-i pp-j)))))
   (let ((faces (reverse '(3 4 1 5)))
	 (ls-ls-i-j (reverse (list (croncher 2 '(0 1 2))
			(croncher '(0 1 2) 0)
			(croncher 0 '(2 1 0))
			(croncher '(2 1 0) 2)))))
     (iter (for face in faces)
	   (for p-face in (cdr faces))
	   (for ls-i-j in ls-ls-i-j)
	   (for p-ls-i-j in (cdr ls-ls-i-j))
	   (appending
	    (iter (for (i j) in ls-i-j)
		 (for (p-i p-j) in p-ls-i-j)
		 (collect (list  (list p-face p-i p-j) (list face i j)))))))))

(defun rotate-ceter-column-down ()
  (let ((faces '(2 1 0 3)))
    (iter (for face in faces)
	  (for p-face in (cdr faces))
	  (appending
	   (iter (for (i j) in (croncher '(0 1 2) 1))
		(collect (list (list face i j) (list p-face i j))))))))

(defun rotate-face-cw-with-view (view)
   (iter (for (c1 c2) in (rotate-face-cw))
	 (collect
	     (list
	      (funcall view c1)
	      (funcall view c2)))))

(defun rotate-center-column-down-with-view (view)
   (iter (for (c1 c2) in (rotate-ceter-column-down))
	 (collect
	     (list
	      (funcall view c1)
	      (funcall view c2)))))

(defun combine-views (&rest views)
  (lambda (c)
    (iter (for v in views)
	  (setq c (funcall v c)))
    c))

(defun mult-view (view n)
  (lambda (c)
    (iter (repeat n)
	  (setq c (funcall view c)))
    c))
(print-cube (make-cube) (mult-view #'view-spin-cube-right 4))
				  

;; ==========================================================
;; cube modifying operations and macros
;; ==========================================================

(defun reverse-operation (operation)
  (mapcar (lambda (pair) (list (cadr pair) (car pair) ))
	  operation))

(defun apply-operation (cube operation)
   (iter (for (c1 c2) in operation)
	 (rotatef
	  (apply #'aref cube c1)
	  (apply #'aref cube c2))))


(defmacro compile-operation (operation)
  `(lambda (cube)
     (declare ((array (mod 128) (6 3 3)) cube)
	      (optimize (speed 3) (safety 0) (space 0)))
     ,@(iter (for (c1 c2) in (eval operation))
	     (collect
		 `(rotatef
		   (aref cube ,@c1)
		   (aref cube ,@c2))))))
	     
(macroexpand-1
 '(compile-operation '(((0 0 0) (0 0 1))
		       ((0 0 1) (0 0 2))
		       ((0 0 2) (0 0 0)))))
(disassemble
 (compile-operation '(((0 0 0) (0 0 1))
		      ((0 0 1) (0 0 2))
		      ((0 0 2) (0 0 0)))))

(defun apply-view (cube view)
  (let ((old-cube (copy-array cube)))
    (iter (for n from 0 below 6)
	  (iter (for i from 0 below 3)
		(iter (for j from 0 below 3)
		      (for c = (list n i j))
		      (rotatef
		       (apply #'aref cube c)
		       (apply #'aref old-cube (funcall view c))))))))

(defun apply-reverse-view (cube view)
  (let ((old-cube (copy-array cube)))
    (iter (for n from 0 below 6)
	  (iter (for i from 0 below 3)
		(iter (for j from 0 below 3)
		      (for c = (list n i j))
		      (rotatef
		       (apply #'aref old-cube (funcall view c))
		       (apply #'aref cube c)))))))


(defun reverse-op (op-name)
  (ecase op-name
    (cw 'ccw)
    (ccw 'cw)
    (lu 'ld)
    (ld 'lu)
    (ru 'rd)
    (rd 'ru)
    (tr 'tl)
    (tl 'tr)
    (br 'bl)
    (bl 'br)))
  

(defun cube-move (cube op-name)
  (funcall
   (ecase op-name
     (cw (compile-operation (rotate-face-cw-with-view #'identity)))
     (ccw (compile-operation (reverse (rotate-face-cw-with-view #'identity))))
     ;; left/rigth column ops
     (lu (compile-operation (reverse (rotate-face-cw-with-view 
				      (combine-views (mult-view #'view-turn-cube-up 3)
						     #'view-spin-cube-right)))))
     (ld (compile-operation (rotate-face-cw-with-view 
			     (combine-views (mult-view #'view-turn-cube-up 3)
					    #'view-spin-cube-right))))
     (ru (compile-operation (rotate-face-cw-with-view 
			     (combine-views #'view-turn-cube-up
					    #'view-spin-cube-right))))
     (rd (compile-operation (reverse (rotate-face-cw-with-view 
				      (combine-views #'view-turn-cube-up
						     #'view-spin-cube-right)))))
     ;; top/bottom row ops
     (tr (compile-operation (reverse (rotate-face-cw-with-view 
				      (mult-view #'view-turn-cube-up 3)))))
     (tl (compile-operation (rotate-face-cw-with-view 
			     (mult-view #'view-turn-cube-up 3))))
     (br (compile-operation (rotate-face-cw-with-view 
			     (mult-view #'view-turn-cube-up 1))))
     (bl (compile-operation (reverse (rotate-face-cw-with-view 
				      (mult-view #'view-turn-cube-up 1))))))
   cube))

(iter (for op in '(cw ccw lu ld ru rd tr tl br bl))
      (let* ((cube (make-cube))
	     (temp (copy-array cube)))
	(cube-move cube op)
	(cube-move cube (reverse-op op))
	(when (not (equalp cube temp))
	  (format t "=====~%cube changed! op: ~a~%" op)
	  (print-cube cube))))

(let* ((cube (make-cube))
       (op 'ru)
       (temp (copy-array cube)))
  (format t "===========~%op: ~A~%" op)
  (print-cube cube)
  (cube-move cube op)
  (print-cube cube)
  (cube-move cube (reverse-op op))
  (when (not (equalp cube temp))
      (format t "=====~%cube changed!~%")
      (print-cube cube)))

(let* ((cube (make-cube)))
  (print-cube cube)
  (cube-move cube 'cw)
  (print-cube cube)
  (cube-move cube 'ru)
  (print-cube cube))

    
(let ((cube (make-cube)))
  (format t "timing!~%~%")
  (time (loop repeat 10000 do
	     (apply-operation cube (reverse (rotate-face-cw-with-view #'identity)))))
  (time (loop repeat 10000 do (cube-move cube 'cw)))
  (time (loop repeat 10000 do
	     (apply-operation cube (reverse (rotate-face-cw-with-view #'view-turn-cube-up)))))
  (time (loop repeat 10000 do (cube-move cube 'bl))))

(defun limited-dfs
    (goal-checker max-depth &optional (opstack nil) (depth 0) (cube (make-cube))))
(defun limited-dfs (goal-checker max-depth &optional (opstack nil) (depth 0) (cube (make-cube)))
  (cond
    ((funcall goal-checker cube)
     (format t "solution: ~A~%" (reverse opstack)))
    ((>= depth max-depth)
     nil)
    (t
     (loop for op in '(cw ccw lu ld ru rd tr br bl)
	do (cube-move cube op)
	do (limited-dfs goal-checker max-depth (cons op opstack) (1+ depth) cube)
	do (cube-move cube (reverse-op op))))))

(defun n->idx (n)
  (multiple-value-bind (face-n x) (floor n 10)
    (multiple-value-bind (i j) (floor x 3)
      (list face-n i j))))

(defmacro cycler-checker-maker (groups)
  (let ((eq-pairs
	 (iter (for g in groups)
	       (appending
		(iter (for a in g)
		      (for b previous a initially (last-elt g))
		      (collect (list a b)))))))
    `(lambda (cube)
       (and ,@(iter (for (a b) in eq-pairs)
		    (collect `(= (aref cube ,@(n->idx a)) ,b)))))))

(limited-dfs (lambda (cube) (= (aref cube 0 0 0) 8)) 3)
(format t "~%")
(limited-dfs
 (lambda (cube)
   (and
    (= (aref cube 0 0 2) 40)
    (= (aref cube 4 0 0) 38)
    (= (aref cube 3 2 2) 02)))
 2)



(format t "~%")
(limited-dfs
 (cycler-checker-maker ((02 00) (40 36) (52 38)))
 5)

(format t "~%")
(limited-dfs
 (cycler-checker-maker ((01 05) (37 43) (03) (07)))
 5)
	  


