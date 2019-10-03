(load "../helpers.lisp") ;; yuck I should really do pacakges

(ql:quickload :iterate) ;; for better iteration macro
(ql:quickload :alexandria) ;; last-elt and other helpers
(defpackage :genetic-algorithm
  (:use :common-lisp :iterate :alexandria))
(in-package :genetic-algorithm)

"
So what to do? Combining two different solutions will be the tricky part,
and it can either be done intelligently, or dumbly. Dumb combination
is more true to nature - you don't get the best qualities of both of 
your parents, you just get a mixed bag and if they aren't good you die.

Something that is not done dumbly in nature is selection of parents. 
Sexual selection means that only genetically compatible (same species) 
parents can reproduce, and fitest individuals get to select fitest mates,
or most compatible mates. Motivated from this, I am going to keep combination
simple and mostly random, but pair selection will be a more complex process,
and extra attention will be paid to speciation.
"

;; A genotype will be expressed as numbers 0-99 in some particular order.
;; using mod 100 instead of one-based counting in case it simplifies some
;; logic, although this may create confusion when viewing genes. Don't let it confuse!

(deftype gene ()
  `(simple-array fixnum *))

	       
(defun p1 (x)
  (format t "~A~%" x))

(defun sqr (x)
  (* x x))

(defun pair-dist (pair1 pair2)
  (sqrt (+ (sqr (- (car pair1) (car pair2)))
	   (sqr (- (cdr pair1) (cdr pair2))))))

(defparameter point-lookup
  (let ((points (cadr (helpers:get-points "Random100.tsp"))))
    (make-array (length points)
		:element-type '(cons fixnum fixnum)
		:initial-contents
		(mapcar (lambda (p) (cons (cadr p) (caddr p))) points))))

(defparameter distance-table
  (make-array (list (length point-lookup) (length point-lookup))
	      :element-type 'single-float
	      :initial-contents
	      (iter (for i below (length point-lookup))
		    (collect
			(iter (for j below (length point-lookup))
			      (collect
				  (pair-dist (aref point-lookup i) (aref point-lookup j))))))))

;; lower bound on possible score
(iter (for i from 0 below 100)
      (summing
       (iter (for j from 0 below 100)
	     (if (not (= i j))
		 (minimizing (aref distance-table i j))))))
(defun gene-cost (gene)
  (iter (for n in-vector gene)
	(for pn previous n initially (last-elt gene))
	(summing (aref distance-table n pn))))

(aref point-lookup 99)
(aref distance-table 0 1)
(gene-cost #(0 1))

(iter (for i from 5 downto 0) (collect i))
(defun shuffled (vec)
  (setq vec (copy-array vec))
  (iter (for i below (length vec))
	(for remlen downfrom (length vec))
	(for swap-i = (+ i (random remlen)))
	(rotatef (aref vec i) (aref vec swap-i)))
  vec)

(shuffled #(a b c d e f g h i j k))
(defun arange (n)
  (make-array n :element-type 'fixnum
	      :initial-contents
	      (iter (for i from 0 below n) (collect i))))
(arange 5)


;; make histogram to check if `shuffled` is
;; implemented correctly. Should be uniform.
(loop for n from 0 below 5
   collect
     (let ((counts (make-array 6 :initial-element 0)))
       (loop repeat 100000 do
	    (incf (aref counts (aref (shuffled #(0 1 2 3 4 5)) n))))
       counts))




(defun check-is-in-subseq (vec el start end)
  (let ((len (length vec)))
    (setq start (mod start len)
	  end (mod end len))
    (iter (for i from start)
	  (for im = (mod i (length vec)))
	  (when (= im end)
	    (finish))
	  (thereis (eql el (aref vec im))))))
(check-is-in-subseq #(a b c d e f g) 'a 3 9)

(position 'd #(a b c))

(sort '(4 3 2 5) #'<)
(iter (for i from 0)
      (for x in '(0 1 2 3 4 5 6 7))
      (if (= i 4)
	  (setq i (+ i 1)))
      (collect i))
(defun cycle-contiguous-p (ls len)
   "rule: allow one break if start and ends with 0 and len"
  (setq ls (sort ls #'<))
  (let ((allowed-break (and (zerop (car ls))
			    (= (last-elt ls) (1- len)))))
    (iter (for n in ls)
	  (for i from (car ls))
	  (for im = (mod i len))
	  (when (and allowed-break (not (= im n)))
	    (setq i n
		  im n
		  allowed-break nil))
	  (always (= im n)))))
(cycle-contiguous-p '(3 5 0 1) 6)

(iter (for base from 0)
      (setq base (mod base 10))
      (if-first-time 
       nil
       (if (= base 0) (finish)))
      (collect base))

(defun cycle-start-end (ls)
  "assumes cycle is sorted numerically"
  (if (< (car ls) (last-elt ls))
      (list (car ls) (last-elt ls))
      (iter (for x in ls)
	    (for px previous x)
	    (for i from (car ls))
	    (if (not (= i x))
		(leave (list x px))))))
(cycle-start-end '(3 4 5))
(cycle-start-end '(8 9 0 1 2 3))
  
(mod -1 5)
(defun find-common-contiguous-subset (vec1 vec2 m)
  "has to be constrained for size m. Return 4 tuple with indices
   Also! Only works for vec2 has no repeate elements!!!"
  ;; base starts from random, so no bias from picking first match
  (let ((base-start (random (length vec1))))
    (iter (for base from base-start)
	  (setq base (mod base (length vec1)))
	  (if-first-time 
	   nil
	   (if (= base base-start) (finish))) ;; all this to do a mod loop
	       
	  (for base-el = (aref vec1 base))
	  (for center = (position base-el vec2))
	  (when (not center) (next-iteration))
	  ;; now we have to check from center-(m-1) to center+m
	  ;; and collect all the indices
	  (thereis
	   (let ((inds
		  (iter (for i from (- center (- m 1)) below (+ center m))
			(for im = (mod i (length vec2)))
			(for el2 = (aref vec2 im))
			(when (check-is-in-subseq vec1 el2 base (+ base m))
			  (collect im)))))
	     (when (and (= (length inds) m)
			(cycle-contiguous-p inds (length vec2)))
	       (destructuring-bind (cycle-start cycle-end) (cycle-start-end inds)
		 (list base (mod (+ base m) (length vec1))
		       (mod cycle-start (length vec2)) (mod (1+ cycle-end) (length vec2))))))))))



(let ((a (shuffled #(a b c d e f g h)))
      (b (shuffled #(a b c d e f g h))))
  (p1 a)
  (p1 b)
  (p1 (find-common-contiguous-subset a b 4)))
;;                               0 1 2 3 4 5 6 7 8 9 0 1 2
(find-common-contiguous-subset #(4 5 6 7 z x y i j k 1 2 3)
			       #(3 4 5 7 a b c d 1 2)
			       4)


(defun crossover-vectors (vec-a vec-b a-start a-end b-start b-end)
  (setq vec-a (copy-array vec-a)
	vec-b (copy-array vec-b))
  (if
   ;; crosses and also checks if subseq's are equal
   (iter (for i-a from a-start)
	 (setq i-a (mod i-a (length vec-a)))
	 (while (not (= i-a a-end)))
	 (for i-b from b-start)
	 (setq i-b (mod i-b (length vec-b)))
	 (while (not (= i-b b-end)))
	 (always (= (aref vec-a i-a) (aref vec-b i-b)))
	 (rotatef (aref vec-a i-a) (aref vec-b i-b)))
   :exact-match
   (list vec-a vec-b)))

	
(defun crossover-m (gene-a gene-b m)
  "returns two children with crossover, or nil if no match"
  (let ((ccs (find-common-contiguous-subset gene-a gene-b m)))
    (if ccs
	(destructuring-bind (a-start a-end b-start b-end) ccs
	  (let ((cv (crossover-vectors gene-a gene-b a-start a-end b-start b-end)))
	    (if (eql cv :exact-match) ;; prevent despots
		nil
		cv))))))


(defun check-gene (gene len)
  (setq gene (sort (copy-array gene) #'<))
  (and
   (= (length gene) len)
   (iter (for i from 0)
	 (for n in-vector gene)
	 (always (= i n)))))

(let ((a (shuffled (arange 100)))
      (b (shuffled (arange 100))))
  (p1 a)
  (p1 b)
  (p1 "  0 1 2 3 4 5 6 7")
  (let ((children (crossover-m a b 4)))
    (iter (for c in children)
	  (if (not (check-gene c 100))
	      (p1 "oops")))
    (format t "~{~A~%~}~%" children)))

(defun find-matches (gene-pool m max-ind)
  (iter (for g1 in-vector gene-pool)
	(for i from 0 below max-ind)
	(appending
	 (iter (for g2 in-vector gene-pool)
	       (for j from 0)
	       (if (= i j) (finish)) ;; upper triangular
	       (let ((children (crossover-m g1 g2 m)))
		 (if children
		     (appending children)))))))

(let* ((n-pop 100)
       (gene-pool
	(make-array n-pop :element-type 'gene
		    :initial-contents
		    (iter (repeat n-pop)
			  (collect (shuffled (arange 100)))))))
  (length (find-matches gene-pool 4 100)))

(let ((points (cadr (helpers:get-points "Random100.tsp"))))
  (list points))

(defun mutation-adjacent-swap (gene)
  (setq gene (copy-array gene))
  (let ((rand-i (random (length gene))))
    (rotatef (aref gene rand-i)
	     (aref gene (mod (1+ rand-i) (length gene))))
    gene))

(defun mutation-random-swap (gene)
  (setq gene (copy-array gene))
  (let ((rand-i (random (length gene)))
	(rand-j (random (length gene))))
    (rotatef (aref gene rand-i)
	     (aref gene rand-j))
    gene))

(mutation-adjacent-swap #(1 2 3 4 5 6))

;; baseline, random population and sort by fitness
(defun best-from-random-sample (n)
  (let ((gene-pool (iter (repeat n)
			 (collect (shuffled (arange 100))))))
    (gene-cost (car (sort gene-pool #'< :key #'gene-cost)))))

(defun random-sample (n)
  (let ((gene-pool (iter (repeat n)
			 (collect (shuffled (arange 100))))))
    (mapcar #'gene-cost (sort gene-pool #'< :key #'gene-cost))))

(best-from-random-sample 5000)
;; ~~4292

(defun vector-to-list (vec)
  (iter (for x in-vector vec)
	(collect x)))

;; next more complex,
;; take best half of solutions and mutuate
(defun simple-mutate (n-pop n-epochs
		      &optional (mutate-fun #'mutation-adjacent-swap))
  (let ((gene-pool
	 (make-array n-pop :element-type 'gene
		     :initial-contents
		     (iter (repeat n-pop)
			   (collect (shuffled (arange 100))))))
	(n-keep (floor n-pop 2)))
    (iter
      (for epoch from 0 to n-epochs)
      (setq gene-pool (sort gene-pool #'< :key #'gene-cost))
      ;;(format t "Epoch ~a, best gene: ~a~%"
	      ;;epoch (gene-cost (first-elt gene-pool)))
      (iter (for i-keep from 0 below n-keep)
	    (for i-kill from n-keep below n-pop)
	    (setf (aref gene-pool i-kill)
		  (funcall mutate-fun (aref gene-pool i-keep)))))
    (setq gene-pool (sort gene-pool #'< :key #'gene-cost))
    (mapcar #'gene-cost (vector-to-list gene-pool))))
  
	
(iter (for i from 30 downto 15) (collect i))

;; next more complex,
;; crossover genes between random parents
"
This method suffers from two things:
The 'dating game' of costs (pop/2)^2 calls to find subseq, 
which costs (len*m)

Any two that match get to make two children. They are then able
to breed with eachother and those children, and so this initial group dominates the 
population. It's like a set of co-fertile creatures are the only ones reproducing.
This could be fixed by allowing asexual reproduction, and }}}}}}}}}
"
(defun mutate-and-crossover (n-pop n-epochs m-crossover &key (verbose nil))
  (let ((gene-pool
	 (make-array n-pop :element-type 'gene
		     :initial-contents
		     (iter (repeat n-pop)
			   (collect (shuffled (arange 100))))))
	(n-keep (floor n-pop 2)))
    (iter
      (for epoch from 0 to n-epochs)
      (setq gene-pool (sort gene-pool #'< :key #'gene-cost))
      ;; mate upper 50%, VERY expensive
      (for children = (find-matches gene-pool m-crossover (floor n-pop 4)))
      (if verbose
	  (format t "Epoch ~a, best gene: ~a,  n children: ~a~%"
		  epoch (gene-cost (first-elt gene-pool)) (length children)))

      ;; first pop/4 children get to live, displacing the worst of the lower 25%
      ;; or all if there are lots of kids
      (iter 
	(for i from (1- n-pop) downto (floor (* n-pop 0.75)))
	(for child in children)
	(setf (aref gene-pool i) child))
      ;; any of the remaining 50% that still lived are replaced with the elite,
      ;; starting with the best gene.
      (iter
	(for i-kill from (- n-pop (length children) 1) downto n-keep)
	(for i-breed from 0)
	(setf (aref gene-pool i-kill) (aref gene-pool i-breed)))
      ;; then check everyone and mutate everyone with 10% probability.
      (iter (for gene in-vector gene-pool)
	    (for i from 0)
	    (when (zerop (random 1))
	      (setf (aref gene-pool i) (mutation-random-swap (aref gene-pool i))))
	    (when (not (check-gene gene 100))
	      (format t "bad gene: ~A ..., len: ~a~%" (subseq gene 0 5) (length gene))
	      ;; kill the offending gene, whatever not a big deal.
	      ;; This happens rarely so this solution is good enough.
	      ;; Don't prevent errors just prevent problems.
	      (setf (aref gene-pool i) (aref gene-pool (random n-pop))))))

    (mapcar #'gene-cost (vector-to-list gene-pool))))


(defun trade-single-point (gene1 gene2)
  " We're looking for gene1 to have (... a ...)")


(p1 (simple-mutate 500 10))

(simple-mutate 100 100 #'mutation-random-swap)
(* 50 50)
(mutate-and-crossover 100 200 6 :verbose t)

(time 5)
(multi-histogram-autolabel
 (list
  ;;(simple-mutate 50 10)
  ;;(time (simple-mutate 100 200 #'mutation-random-swap))
  (time (mutate-and-crossover 100 100 3))
  (time (mutate-and-crossover 100 100 5))
  (time (mutate-and-crossover 100 100 8))
       )
 50)


"Records:
1599 with (mutate-and-crossover 100 1000 3) in 16 seconds
1328 with (simple-mutate 100 8000 #'mutation-random-swap) in 35 seconds
1099 with (simple-mutate 1000 8000 #'mutation-random-swap) in 607 seconds
"
