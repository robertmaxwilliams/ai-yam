(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :cl-interpol)
(cl-interpol:enable-interpol-syntax)

(load "graphics.lisp")

(defpackage :tsp-viewer
  (:use :cl :iterate :alexandria :max-graphics))
(in-package :tsp-viewer)

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

(defparameter *nth-points-file* 4)
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
	      (format nil "/home/max/ai-yam/Project1/Random~a_solution.tsp" n))))))
  (move-global-points 0)) ;;init


(defun render ()
  (gl:clear :color-buffer)
  (iter (for p1 in points)
   (for p2 previous p1 initially (last-elt points))
   (for (n1 x1 y1) = p1)
   (for (n2 x2 y2) = p2)
   (draw-circle x1 y1 4 :white)
   (draw-arrow-with-offsets x1 y1 x2 y2 2 5 4 4 :blue)
   ;;(draw-number (round x1) x1 (- y1 5) 4 :green) ;; optional display x and y
   ;;(draw-number (round y1) x1 (- y1 10) 4 :green ;; for debug
     (draw-number n1 x1 y1 4 :black))
   (draw-number *nth-points-file* -5 0 8 :green)
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
       (:scancode-left (move-global-points -1))
       (:scancode-right (move-global-points 1))
       (:scancode-escape (sdl2:push-event :quit))))
    (:quit () t)))

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
        (main-loop win #'render)))))

(main)
