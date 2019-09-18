
;; Load required libraries.
(ql:quickload :sdl2)
(ql:quickload :cl-opengl)
(ql:quickload :iterate)
(ql:quickload :alexandria)

(ql:quickload :cl-interpol)
(cl-interpol:enable-interpol-syntax)

(defpackage :max-graphics
  (:use :cl :iterate :alexandria)
  (:export "MAIN" "DRAW-CIRCLE" "DRAW-NUMBER" "DRAW-LINE"
	   "DRAW-CIRCLE" "DRAW-ARROW-WITH-OFFSETS" "DRAW-ARROW"
	   "DEBUG-LOG" "SETUP-GL"))
(in-package :max-graphics)

(defparameter points
  '((1 87.951294 2.658162) (2 33.4666 66.682945) (3 91.77831 53.807182) (4 20.526749 47.63329)))

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

(defun error-if-null (x)
  (assert x)
  x)

(let  ((colors-alist
	'((:black 0 0 0)
	  (:white 1.0 1.0 1.0)
	  (:red 1.0 0 0)
	  (:green 0 1.0 0)
	  (:blue 0 0 1.0)
	  (:yellow 1.0 1.0 0)
	  (:cyan 0 1.0 1.0)
	  (:magenta 1.0 0 1.0))))
  (defun get-color-from-name (name)
    (cond
      ((atom name)
       (append (cdr (error-if-null (assoc name colors-alist))) '(0.2)))
      ((= (length name) 3) (append name '(0.2)))
      ((= (length name) 4) name)
      (t (error "bad color definition"))))
  (defun set-color (color)
    (apply #'gl:color (get-color-from-name color))))

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

(defun seven-segment (x y height codes &optional (color :green))
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
  (defun draw-number (number x y height &optional (color :green))
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


(defun draw-line (x1 y1 x2 y2 thickness &optional (color :white))
  (set-color color)
  (gl-with :polygon
    (destructuring-bind (dx dy) (*-ls (orthagonal-unit x1 y1 x2 y2) (/ thickness 2))
      (gl:vertex x1 y1)
      (gl:vertex x2 y2)
      (gl:vertex (+ x2 dx) (+ y2 dy))
      (gl:vertex (+ x1 dx) (+ y1 dy))
      )))


(defun l2 (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defun draw-arrow (x1 y1 x2 y2 thickness arrow-depth &optional (color :white))
  (set-color color)
  (let ((length (l2 x1 y1 x2 y2)))
    (if (< length arrow-depth)
	(setq arrow-depth (* 0.5 length))))
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
				offset-source offset-destination &optional (color :white))
  (let* ((p-unit (parallel-unit x1 y1 x2 y2))
	 (d-source (*-ls p-unit offset-source))
	 (d-destination (*-ls p-unit (* -1 offset-destination))))
    (destructuring-bind ((dx1 dy1) (dx2 dy2)) (list d-source d-destination)
      (draw-arrow (+ x1 dx1) (+ y1 dy1) (+ x2 dx2) (+ y2 dy2) thickness arrow-depth color))))

(defun draw-circle (x y radius &optional (color :white) (n-segments 50))
  (set-color color)
  (gl-with :polygon
    (iter (for i from 0 below n-segments)
	  (for rad = (* 2 pi (/ i n-segments)))
	  (gl:vertex (+ x (* radius (sin rad))) (+ y (* radius (cos rad)))))))


;; =================================================================================================== 
;; copy these into your program and modify
;; =================================================================================================== 
(defparameter red-x 50)
(defparameter red-y 50)

(defun render ()
  (gl:clear :color-buffer)
   (draw-circle 10 10 4 :white)
   (draw-arrow-with-offsets 10 10 red-x red-y 2 5 4 4 :blue)
   (draw-circle red-x red-y 4 :red)
  
   (draw-number red-x -5 0 8 :green)
   (draw-number red-y 10 0 8 :green)
   (gl:flush))

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
       (:scancode-left (decf red-x 10))
       (:scancode-right (incf red-x 10))
       (:scancode-up (incf red-y 10))
       (:scancode-down (decf red-y 10))
       (:scancode-escape (sdl2:push-event :quit))))
    (:quit () t)))

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
;;(main)
