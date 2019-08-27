
(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :cl-interpol)
(ql:quickload :sdl2)
(ql:quickload :cffi)
(cl-interpol:enable-interpol-syntax)




(load "graphics.lisp")

(defpackage :mandy
  (:use :cl :iterate :alexandria :max-graphics :cffi))
(in-package :mandy)

(defparameter *max-iter* 15)
(defparameter *size* 100)
(defparameter offset-x 0)
(defparameter offset-y 0)
(defparameter zoom 1.0)

(defun iter-to-color (i)
  (declare (fixnum i))
  (let ((f (floor (* 255 i) *max-iter*)))
    (list f f f 255)))

(defun magic-color (z)
  (if (zerop (imagpart z))
      0
      (truncate (* 255 (tanh (abs (* .001 (/ (realpart z) (imagpart z)))))))))

(defun complex-angle (c)
  (if (zerop (realpart c))
      0
      (truncate (* 255 (mod (/ (atan (/ (imagpart c) (realpart c))) (* 2 pi)) 1)))))
(complex-angle #c(0 -1))

(defun z-to-color (z)
  (let ((f1 (complex-angle z))
	(f2 (magic-color z)))
    (list f1 0 f2 255)))

(defun mandy-iter (z max-iter)
  (declare (optimize (speed 3) (safety 0)))
  (declare ((complex single-float) z))
  (let ((c z))
    (declare ((complex single-float) c))
    (iter
      (for i from 0 below max-iter)
      (declare (fixnum i))
      (setq z (+ (* z z) c))
      (if (>= (the single-float (abs z)) (the single-float 2.0))
	  (return (iter-to-color i))
      (finally (return (z-to-color z)))))))



(defun get-mandy-color (x y max-x max-y)
  ;;(declare (optimize (speed 3) (safety 0)))
  ;;(declare (fixnum x y max-x max-y))
  (mandy-iter (complex
	       (float (/ x max-x))
	       (float (/ y max-y)))
	      *max-iter*))

(defun get-mandy-color-fake (x y max-x max-y)
  (mapcar #'truncate (list (mod (/ x max-x) 256) (mod (/ y max-y) 256) 0 255)))

(let ((x 0))
  (defun has-offset-zoom-changed ()
    (let* ((hash (sxhash (list *size* *max-iter* offset-x offset-y zoom)))
           (x-is-same (= x hash)))
      (setq x hash)
      (not x-is-same))))
(has-offset-zoom-changed)
(setq zoom 1.2)
(complex-angle #c(0 0))

(defun blit-mandy-to-window (window)
  (let ((size *size*))
    (with-foreign-object (pixel-array :uchar (* size size 4))
      (loop for i below size
	 do (loop for j below size
	       do (let ((rgba (get-mandy-color
			       (- (/ (* i 600) size) 300 offset-x)
			       (- (/ (* j 600) size) 300 offset-y)
			       (* zoom 3 100)
			       (* zoom 3 100))))
		    (loop for k upfrom 0
		       for b-color in rgba
		       do (setf (mem-aref pixel-array :uchar (+ (* i 4) (* j size 4) k)) b-color)))))
      (let ((mandy-surf (sdl2:create-rgb-surface-from
			 (mem-aptr pixel-array :uchar)
			 size size (* 4 8) (* size 4)
			 :a-mask #xff000000 :b-mask #x00ff0000 :g-mask #x0000ff00 :r-mask #x000000ff)))
	(sdl2:blit-scaled
	 mandy-surf (sdl2:make-rect 0 0 size size)
	 (sdl2:get-window-surface window) (sdl2:make-rect 0 0 600 600))))))


(get-mandy-color 1 1 2 2)

(defun draw-mandy (rend)
  (loop for x from 0 below 600 by 6
     do (loop for y from 0 below 600 by 6
	   do (apply 
	       #'sdl2:set-render-draw-color rend
	       (get-mandy-color
		(- x 300 offset-x)
		(- y 300 offset-y)
		(* zoom 300)
		(* zoom 300)))
	   do (loop for dx from 0 below 6
		 do (loop for dy from 0 below 6
		       do (sdl2:render-draw-point rend (+ x dx) (+ y dy)))))))


(defun zoom-mult (n)
  (setq zoom (* zoom n))
  (setq offset-x (* offset-x n))
  (setq offset-y (* offset-y n)))

(defparameter move-amount 100)

(defmacro decf+ (place)
  `(if (> ,place 1)
       (decf ,place)
       ,place))

(defun main-loop (win rend)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (declare (ignorable win rend))
  (sdl2:with-event-loop (:method :poll)
    (:idle
     ()

     (if (has-offset-zoom-changed)
	 (progn
	   (blit-mandy-to-window win)
	   (sdl2:update-window win))
	 (sdl2:delay 50)))
     ;;(draw-mandy rend)
     ;;(sdl2:render-present rend)
    (:keydown
     (:keysym keysym)
     (format t "Key pressed: ~a~%" (sdl2:scancode keysym))
     (case (sdl2:scancode keysym)
       (:scancode-w (incf offset-y move-amount))
       (:scancode-s (incf offset-y (- move-amount)))
       (:scancode-a (incf offset-x move-amount))
       (:scancode-d (incf offset-x (- move-amount)))
       (:scancode-q (zoom-mult 0.9))
       (:scancode-e (zoom-mult 1.1111))
       (:scancode-1 (setq *size* 50))
       (:scancode-2 (setq *size* 100))
       (:scancode-3 (setq *size* 300))
       (:scancode-4 (setq *size* 600))
       (:scancode-equals (format t "max iter: ~A~%" (incf *max-iter*)))
       (:scancode-minus (format t "max iter: ~A~%" (decf+ *max-iter*)))
       (:scancode-escape (sdl2:push-event :quit))))
    (:quit () t)))

(let ((a 4))
  (decf a))

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
      (sdl2:with-renderer (rend win)
	(sdl2:with-gl-context (gl-context win)
	  ;; Basic window/gl setup
	  (setup-gl win gl-context)

	  ;; Run main loop
	  (main-loop win rend))))))

(format t "~A~%" (/ (1+ (sqrt 5)) 2))
(format t "~{~A~%~}"
	(iter (for i from 0 to 12)
	      (collect (expt 2 (/ i 12)))))
