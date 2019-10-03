
(in-package :genetic-algorithm)

(ql:quickload :vgplot)

(vgplot:plot '(1 2 3) '(0 -2 17))

(defun strip-specials (str)
  (map 'string (lambda (c)
		 (if (member c (list #\# #\')) #\. c))
       str))

(defun histogram (vals &optional (bins 10))
  (let* ((min-val (apply #'min vals))
	 (max-val (apply #'max vals))
	 (range (- max-val min-val))
	 (step (/ range bins))
	 (bin-arr (make-array bins)))
    (iter (for v in vals)
	  (for i = (floor (- v min-val) step))
	  (when (>= i bins)
	    (format t "~a not in ~a~%" i bins)
	    (next-iteration))
	  (incf (aref bin-arr i)))
    (vgplot:bar :y `((,bin-arr :label "oops"))
		:x (iter (for i below bins)
			 (collect
			     (format nil "~a - ~a"
				   (floor (+ min-val (* i step)))
				   (floor (+ min-val (* (1+ i) step)))))))
    ;;(vgplot:axis '(t t -0.5 1))
    (vgplot:xlabel "cost")
    (vgplot:ylabel "occurences in bin")))

(defun multi-histogram (vals-ls labels &optional (bins 10))
  (let* ((all-vals (apply #'append vals-ls))
	 (n-plots (length vals-ls))
	 (min-val (apply #'min all-vals))
	 (max-val (apply #'max all-vals))
	 (range (- max-val min-val))
	 (step (/ range bins))
	 (bin-arr-ls (iter (repeat n-plots) (collect (make-array bins))))
	 (max-labels 20) 
	 (plot-label-skip (floor bins max-labels)))
    (iter (for bin-arr in bin-arr-ls)
	  (for vals in vals-ls)
	  (for i from 0)
	  (format t "~a~%" i)
	  (iter (for v in vals)
		(for i = (floor (- v min-val) step))
		(when (>= i bins)
		  (format t "~a not in ~a~%" i bins)
		  (next-iteration))
		(incf (aref bin-arr i))))
    (vgplot:bar :y (mapcar (lambda (x l) (list x :label l))
			   bin-arr-ls labels)
		:x (iter (for i below bins)
			 (collect
			     (if (zerop (mod i plot-label-skip))
				 (floor (+ min-val (* i step)))
				 " "))))
    ;;(vgplot:axis '(t t -0.5 1))
    (vgplot:xlabel "cost")
    (vgplot:ylabel "occurences in bin")))


;;(histogram (simple-mutate 500 10))
;;(histogram (random-sample 5000))

(defmacro multi-histogram-autolabel (things-ls bins)
  `(multi-histogram
    ,things-ls
    ,(cons 'list
	   (mapcar (lambda (thing) (strip-specials (format nil "~a" thing)))
		   (cdr things-ls)))
    ,bins))

(multi-histogram-autolabel
 (list 
  (simple-mutate 500 10)
  (random-sample 5000))
 50)

(histogram (time
	    (simple-mutate 100 500 #'mutation-random-swap)))

(simple-mutate 50 200 #'mutation-adjacent-swap)
(multi-histogram-autolabel
 (list 
  (simple-mutate 50 5 #'mutation-random-swap)
  (simple-mutate 50 50 #'mutation-random-swap)
  (simple-mutate 50 5 #'mutation-adjacent-swap)
  (simple-mutate 50 50 #'mutation-adjacent-swap)
  )
 50)

(multi-histogram-autolabel
 (list 
  (simple-mutate 50 5 #'mutation-random-swap)
  (simple-mutate 50 50 #'mutation-random-swap)
  (simple-mutate 200 500 #'mutation-random-swap)
  (simple-mutate 200 1000 #'mutation-random-swap)
  )
 50)
