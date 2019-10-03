;; compile a DFA into a computed GOTO

(let ((fuck nil))
  (pop fuck))


(defun dfa-1 (instructions)
  (let ((brancher nil))
    (tagbody
       (setq brancher 
	     (make-array '(3 3)
			 :element-type 'function
			 :initial-element (lambda () (error "undefined shit happened"))
			 :initial-contents 
			 (list   ;; reserved symbol 0,   symbol 1,        symbol 2
			  (list (lambda () (go :end-0)) (lambda () (go 1)) (lambda () (go 2))) ;; state 0 
			  (list (lambda () (go :end-1)) (lambda () (go 1)) (lambda () (go 2))) ;; state 1
			  (list (lambda () (go :end-2)) (lambda () (go 2)) (lambda () (go 1))) ;; state 2
			  )))
     0
       (funcall (aref brancher 0 (pop instructions)))
     1
       (funcall (aref brancher 1 (pop instructions)))
     2
       (funcall (aref brancher 2 (pop instructions)))
     :end-0
       (format t "0~%") (go :end)
     :end-1
       (format t "1~%") (go :end)
     :end-2
       (format t "2~%") (go :end)
     :end
       )))
   
(dfa-1 '(1 2 2 1 0))
(disassemble #'dfa-1)
		  

   

(defun fuckm (n)
  (declare (optimize  (speed 3) (safety 0))
	   (fixnum n))
  (case (mod n 4)
    (0 'foo)
    (1 'bar)
    (2 'rosco)
    (3 'noptop)))
(defun fuck-you (n)
  (declare (optimize  (speed 3) (safety 0))
	   (fixnum n))
  (let ((foo (mod n 4))
	(result nil))
    (tagbody
       (go 3)
     0 (setf result 'foo) (go :end)
     1 (setf result 'bar) (go :end)
     2 (setf result 'rosco) (go :end)
     3 (setf result 'noptop) (go :end)
     :end)
    result))
       
(fuckm 3)
(disassemble #'fuckm)
