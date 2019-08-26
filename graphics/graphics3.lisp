;; Load required libraries.
(ql:quickload :sdl2)
(ql:quickload :cl-opengl)
(ql:quickload :iterate)
(ql:quickload :alexandria)

(ql:quickload :cl-interpol)
(cl-interpol:enable-interpol-syntax)

(defpackage :max-graphics
  (:use :cl :iterate :alexandria))

(in-package :max-graphics)
(defun main ())
(in-package :cl-user)
(defun main ()
  (max-graphics::main))
(in-package :max-graphics)

(defparameter points  
  '((1 87.951294 2.658162) (2 33.4666 66.682945) (3 91.77831 53.807182) (4 20.526749 47.63329)))


;; <<<<<<<<<<<<<<<<<<<<<<<< SHOULD BE IMPORT SORRY

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

;; >>>>>>>>>>>>>>>>>>>>>..

(defun slide-and-scale-points (points)
  (destructuring-bind (min-x min-y max-x max-y)
      (iter (for (n x y) in points)
	    (minimizing x into min-x)
	    (minimizing y into min-y)
	    (maximizing x into max-x)
	    (maximizing y into max-y)
	    (finally (return (list min-x min-y max-x max-y))))
    (iter (for (n x y) in points)
	  (collect (list n
			 (* (/ (- x min-x) (- max-x min-x)) 100)
			 (* (/ (- y min-y) (- max-y min-y)) 100))))))

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))

(defparameter *min* -10)
(defparameter *max* 110)
 
(defun setup-gl (win gl-context)
  "Setup OpenGL with the window WIN and the gl context of GL-CONTEXT"
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 600 600)
  (gl:matrix-mode :projection)
  (gl:ortho *min* *max* *min* *max* *min* *max*)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))

(let  ((colors-alist
	'((black 0 0 0)
	  (white 1.0 1.0 1.0)
	  (red 1.0 0 0)
	  (green 0 1.0 0)
	  (blue 0 0 1.0)
	  (yellow 1.0 1.0 0)
	  (cyan 0 1.0 1.0)
	  (magenta 1.0 0 1.0))))
  (defun get-color-from-name (name)
    (append (cdr (assoc name colors-alist)) '(255)))
  (defun set-color (color)
    (apply #'gl:color (get-color-from-name color))))

(defun draw-digit (n x y)
  (let ((h 10)
	(w 10))
    (case n
      (1 ()))))

(defun draw-two-triangles ()
  (gl:begin :triangles)
  (gl:vertex 0 0)
  (gl:vertex 0.5 0.5)
  (gl:vertex 0 0.5)
  (gl:vertex 0.8 0.8)
  (gl:vertex 0.85 0.85)
  (gl:vertex 0.85 0.91)
  (gl:end))

(defun draw-small-tringle-on-point (x y color)
  (gl:begin :triangles)
  (set-color color)
  (let ((dx 10) (dy 10))
    (gl:vertex x y)
    (gl:vertex (+ x dx) (+ y dy))
    (gl:vertex (- x dx) (+ y dy)))
  (gl:end))

(defmacro gl-with (symbol &body body)
  `(progn
     (gl:begin ,symbol)
     ,@body
     (gl:end)))

(defun center-rect (x y w h)
  (gl-with :polygon
    (let ((x1 (- x (/ w 2)))
	  (x2 (+ x (/ w 2)))
	  (y1 (- y (/ h 2)))
	  (y2 (+ y (/ h 2))))
      (gl:vertex x1 y1)
      (gl:vertex x1 y2)
      (gl:vertex x2 y2)
      (gl:vertex x2 y1))))
  
(defun seven-segment (x y height codes &optional (color 'green))
  (set-color color)
  (let* ((width (* height 0.4))
	 (bar-length (* width 0.8))
	 (thickness (/ height 10)))
    (iter (for c in codes)
	  (ccase c
	    (a (center-rect x (+ y (/ height 2)) bar-length thickness))
	    (b (center-rect (+ x (/ width 2)) (+ y (/ height 4)) thickness bar-length))
	    (c (center-rect (+ x (/ width 2)) (- y (/ height 4)) thickness bar-length))
	    (d (center-rect x (- y (/ height 2)) bar-length thickness))
	    (e (center-rect (- x (/ width 2)) (- y (/ height 4)) thickness bar-length))
	    (f (center-rect (- x (/ width 2)) (+ y (/ height 4)) thickness bar-length))
	    (g (center-rect x y bar-length thickness))))))

(defun decimal-pop (number)
  "returns first digit and rest of number"
  (assert (>= number 0))
  (multiple-value-bind (a b)
      (floor number (expt 10 (floor (log number 10))))
    (list a b)))

(decimal-pop 10)

(let ((seven-segment-codes
       '((1 b c)
	 (2 a b g e d)
	 (3 a b g c d)
	 (4 f g b c)
	 (5 a f g c d)
	 (6 a f e d c g)
	 (7 a b c)
	 (8 a b c d e f g)
	 (9 a b c d f g)
	 (0 a b c d e f))))
  (defun draw-number (number x y height &optional (color 'green))
    (cond
      ((< number 10)
	  (seven-segment x y height (cdr (assoc number seven-segment-codes)) color))
      (t (let ((foo (decimal-pop number)))
	   (draw-number (car foo) x y height color)
	   (draw-number (cadr foo) (+ x (* height 0.55)) y height color))))))

(defun norm (x y)
  (sqrt (+ (expt x 2) (expt y 2))))

(defun parallel-unit (x1 y1 x2 y2)
  (let* ((x (- x2 x1))
	 (y (- y2 y1))
	 (len (norm x y))
	 (unit-x (/ x len))
	 (unit-y (/ y len)))
    (list unit-x unit-y)))

(defun orthagonal-unit (x1 y1 x2 y2)
  (destructuring-bind (x y) (parallel-unit x1 y1 x2 y2)
    (list y (- x))))

(defun *-ls (ls scalar)
  (mapcar (lambda (x) (* x scalar)) ls))

(defun draw-arrow (x1 y1 x2 y2 thickness arrow-depth &optional (color 'white))
  (set-color color)
  (gl-with :polygon
    (destructuring-bind (dx dy) (*-ls (orthagonal-unit x1 y1 x2 y2) (/ thickness 2))
      (destructuring-bind (dx2 dy2) (*-ls (parallel-unit x1 y1 x2 y2) (- arrow-depth))
	(gl:vertex (+ x1 dx) (+ y1 dy))
	(gl:vertex (+ x2 dx dx2) (+ y2 dy dy2))
	;;(gl:vertex (+ x2 (* 2 dx) dx2) (+ y2 (* 2 dy) dy2))
	(gl:vertex x2 y2)
	(gl:vertex (+ x2 (- dx) dx2) (+ y2 (- dy) dy2))
	(gl:vertex (+ x1 (- dx)) (+ y1 (- dy)))
	))))

(defun draw-arrow-with-offsets (x1 y1 x2 y2 thickness arrow-depth
				offset-source offset-destination &optional (color 'white))
  (let* ((p-unit (parallel-unit x1 y1 x2 y2))
	 (d-source (*-ls p-unit offset-source))
	 (d-destination (*-ls p-unit (* -1 offset-destination))))
    (destructuring-bind ((dx1 dy1) (dx2 dy2)) (list d-source d-destination)
      (draw-arrow (+ x1 dx1) (+ y1 dy1) (+ x2 dx2) (+ y2 dy2) thickness arrow-depth color))))

	
(defun draw-circle (x y radius &optional (color 'white) (n-segments 50))
  (set-color color)
  (gl-with :polygon
    (iter (for i from 0 below n-segments)
	  (for rad = (* 2 pi (/ i n-segments)))
	  (gl:vertex (+ x (* radius (sin rad))) (+ y (* radius (cos rad)))))))

(defun render ()
  (gl:clear :color-buffer)
  (iter (for p1 in points)
	(for p2 previous p1 initially (last-elt points))
	(for (n1 x1 y1) = p1)
	(for (n2 x2 y2) = p2)
	(draw-circle x1 y1 4 'white)
	(draw-arrow-with-offsets x1 y1 x2 y2 2 5 4 4 'blue)
	;;(draw-number (round x1) x1 (- y1 5) 4 'green) ;; optional display x and y
	;;(draw-number (round y1) x1 (- y1 10) 4 'green ;; for debug
	(draw-number n1 x1 y1 4 'black))
  (draw-number *nth-points-file* -5 0 8 'green)

  (gl:flush))
(main)

(let ((n 4))
  (defparameter *nth-points-file* n)
  (defun move-global-points (dn)
    (incf n dn)
    (if (> n 12) (setq n 12))
    (if (< n 4) (setq n 4))
    (setq *nth-points-file* n)
    (defparameter points
      (slide-and-scale-points
       (cadr (get-points
	      (format nil "/home/max/ai-yam/Project1/Random~a_solution.tsp" n)))))))

  
 
(defun main-loop (win render-fn)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
      (funcall render-fn)
      ;; Swap back buffer
      (sdl2:gl-swap-window win)
      (sdl2:delay 33))

    (:keydown
     (:keysym keysym)
     (format t "Key pressed: ~a~%" (sdl2:scancode keysym))
     (case (sdl2:scancode keysym)
       (:scancode-left (move-global-points -1))
       (:scancode-right (move-global-points 1))
       (:scancode-escape (sdl2:push-event :quit))))

    (:quit () t)))

(main)

(defun main ()
  "The entry point of our game."
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
        (main-loop win #'render)))))


(defun draw-nth-solution (n)
  "nasty globals but thats how it be"
  (defparameter points
    (slide-and-scale-points
     (cadr (get-points
	    (format nil "/home/max/ai-yam/Project1/Random~a_solution.tsp" n)))))
  (main))

(let ((n 4))
(draw-nth-solution 4)
(draw-nth-solution 5)
(draw-nth-solution 6)
(draw-nth-solution 7)
(draw-nth-solution 8)
(draw-nth-solution 9)
(draw-nth-solution 10)
(draw-nth-solution 11)
(draw-nth-solution 12)
