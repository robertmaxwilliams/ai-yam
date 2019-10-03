(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :cl-interpol)
(cl-interpol:enable-interpol-syntax)

(load "graphics.lisp")

(defpackage :edge-selection-viewer
  (:use :cl :iterate :alexandria :max-graphics))
(in-package :edge-selection-viewer)

(defmacro for-in-file-lines ((var-name file-name) &body body)
  (let ((in (gensym)))
    `(let ((,in (open ,file-name :if-does-not-exist nil)))
       (when ,in
	 (loop for ,var-name = (read-line ,in nil)
	    while ,var-name do (progn ,@body)))
	 (close ,in))))

(defun get-points (filename)
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

(defun scale-points-maker (f)
  (lambda (p)
    (list (car p) (* f (cadr p)) (* f (caddr p)))))

(defun scale-args-maker (f)
  (lambda (funarg)
    (cons (car funarg) (mapcar (scale-points-maker f) (cdr funarg)))))


   
(defun interpolate-between-points (p1 p2 s)
  (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list p1 p2)
    (declare (ignore n1 n2))
    (list
     -1
     (+ x1 (* (- x2 x1) s))
     (+ y1 (* (- y2 y1) s)))))

(defun closest-point-on-line (p0 p1 p2)
  (destructuring-bind ((n0 x0 y0) (n1 x1 y1) (n2 x2 y2)) (list p0 p1 p2)
    (declare (ignore n0 n1 n2))
    (let* ((x21 (- x2 x1))
	   (y21 (- y2 y1))
	   (x01 (- x0 x1))
	   (y01 (- y0 y1)))
      (interpolate-between-points
       p1 p2
       (/
	(+ (* x01 x21) (* y01 y21))
	(+ (* x21 x21) (* y21 y21)))))))

(defun trim (x lower upper)
  (cond ((< x lower) lower)
	((> x upper) upper)
	(t x)))

(defparameter cs 1)

(defun @ (multilambda key &rest args)
    (let ((fun (getf multilambda key)))
      (if fun
	  (apply fun args)
	  (format t "OOPS! ~a is not a fun!" key))))



(defun coord-distance (x1 x2 y1 y2)
  "mathy distance function"
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
(defun distance (p1 p2)
  "eucledian distance between two (n x y) points"
  (destructuring-bind ((n1 x1 y1) (n2 x2 y2)) (list p1 p2)
    (declare (ignore n1 n2))
    (coord-distance x1 x2 y1 y2)))
(defun cost (cycle)
  (iter (for p1 in cycle)
	(for p2 previous p1 initially (last-elt cycle))
	(summing (distance p1 p2))))

(defun controller-maker ()
  (let* ((filename-n-points 30)
	 (filename-starting-point 1)
	 (file-prefix "near")
	 (ni 0)
	 (nj 0)
	 (instructions nil)
	 (instructions-list nil)
	 (self nil))
    ;;(format t "~%~%~A~%~%" instructions-list)
    (setq
     self
     (list
      :load-file
      (lambda (n-points starting-point)
	(format t "filename : ~A~%" 
		(format nil "/home/max/ai-yam/project3/outfiles/~a-~A-~A.sexp"
			file-prefix n-points starting-point))
	(let ((result nil))
	  (with-open-file
	      (f
	       (format nil "/home/max/ai-yam/project3/outfiles/~a-~A-~A.sexp"
		       file-prefix n-points starting-point))
	    (iter
	      (for x = (read f nil :eof))
	      (with top = nil)
	      (cond
		((eql x :eof) (finish))
		((and (atom x) (string= x 'end))
		 (push (reverse top) result) (setq top nil))
		(t (push x top)))))
	  (if (null result) (format t "controller-maker result is null!~%")
	      (reverse result))))

      :file-selector
      (lambda (dn)
	(setf filename-n-points (trim (+ filename-n-points (* 10 dn)) 30 40))
	(@ self :starting-point-selector 0))

      :file-prefix-toggle
      (lambda ()
	(setq file-prefix (if (string= file-prefix "rand") "near" "rand")))

      :starting-point-selector
      (lambda (dn)
	(setq filename-starting-point (trim (+ filename-starting-point dn) 0 (1- filename-n-points)))
	(setq instructions-list (@ self :load-file filename-n-points filename-starting-point))
	(format t "len instructions-list: ~a ~a~%"
		(length (@ self :load-file filename-n-points filename-starting-point))
		(length instructions-list))
	(@ self :pane-selector 0))

      :pane-selector 
      (lambda (dn)
	"dn is change in nth file then reads file"
	(let ((combined (+ (* 2 ni) nj)))
	  (incf combined dn)
	  (setf ni (floor combined 2))
	  (setf nj (mod combined 2)))
	(when (< ni 0) (setq nj 0) (setq ni 0))
	(when (>= ni (length instructions-list))
	  (setq ni (max 0 (1- (length instructions-list)))))
	(format t "ni: ~A, nj: ~A~%" ni nj)
	(setq instructions (nth ni instructions-list))
	(format t "n-points: ~a, starting-point: ~a, ~%  len instructions: ~a, len instructions-list:, ~a~%"
		filename-n-points filename-starting-point (length instructions) (length instructions-list)))

      :render 
      (lambda ()
	(gl:clear :color-buffer)
	(iter (for funargs in instructions)
	      (setf (car funargs) (intern (string (car funargs)) :keyword))

	      (destructuring-case funargs
		((:points p-ls)
		 (iter (for (n x y) in p-ls)
		       (draw-circle x y cs :white)
		       (draw-number n x y cs :black)))

		((:cycle p-ls)
		 (draw-number (floor (cost p-ls)) 0 -6 6)
		 (iter (for p1 in p-ls)
		       (for p2 previous p1 initially (last-elt p-ls))
		       (for (n1 x1 y1) = p1)
		       (for (n2 x2 y2) = p2)
		       (draw-circle x1 y1 cs :green)
		       (draw-number n1 x1 y1 cs :black)
		       (draw-arrow-with-offsets x1 y1 x2 y2 2 5 cs cs '(0.2 1 0.2))))

		((:arrow (n0 x0 y0) (n1 x1 y1))
		 (declare (ignore n0 n1))
		 (when nil
		   (draw-arrow-with-offsets x0 y0 x1 y1 2 5 cs cs '(0.0 0.5 1.0 0.1))))
		((:best-arrow (n0 x0 y0) (n1 x1 y1))
		 (declare (ignore n0 n1))
		 (when (= nj 1)
		   (draw-arrow-with-offsets x0 y0 x1 y1 0.5 1 cs cs :blue)))
		((:best-best-arrow (n0 x0 y0) (n1 x1 y1))
		 (declare (ignore n0 n1))
		 (when (= nj 1)
		   (draw-arrow-with-offsets x0 y0 x1 y1 1 1 cs cs :cyan)))))
	(gl:flush))))
    (@ self :file-selector 0) ;; preset
    self))

(defun main-loop (win)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (let ((controller (controller-maker)))
    (sdl2:with-event-loop (:method :poll)
      (:idle ()
	     (@ controller :render)
	     ;; Swap back buffer
	     (sdl2:gl-swap-window win)
	     (sdl2:delay 33))
      (:keydown
       (:keysym keysym)
       (format t "Key pressed: ~a~%" (sdl2:scancode keysym))
       (case (sdl2:scancode keysym)
	 (:scancode-left (@ controller :pane-selector -1))
	 (:scancode-right (@ controller :pane-selector 1))
	 (:scancode-up (@ controller :starting-point-selector 1))
	 (:scancode-down (@ controller :starting-point-selector -1))
	 (:scancode-1 (@ controller :file-selector -1))
	 (:scancode-2 (@ controller :file-selector 1))
	 (:scancode-3 (@ controller :pane-selector -100))
	 (:scancode-4 (@ controller :pane-selector 100))
	 (:scancode-5 (@ controller :file-prefix-toggle))
	 (:scancode-escape (sdl2:push-event :quit))))
      (:quit () t))))

(defun main (&optional argv)
  "The entry point of our game."
  (declare (ignore argv))
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
	       sdl2-ffi:+sdl-major-version+
	       sdl2-ffi:+sdl-minor-version+
	       sdl2-ffi:+sdl-patchlevel+)

    (sdl2:with-window (win :title "Yum"
			   :w 600 :h 600
			   :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	;; Basic window/gl setup
	(setup-gl win gl-context)

	;; Run main loop
	(main-loop win)))))

(progn
  (main))
