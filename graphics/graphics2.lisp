(ql:register-local-projects) 
(ql:quickload 'mcclim)
(ql:quickload 'uiop)
(ql:quickload 'clim-listener)
(clim-listener:run-listener :new-process t)

(ql:quickload "clim-examples")   ; Load the system with examples.
(clim-demo:demodemo) 

(in-package climi)

(in-package :clim-user)

(defparameter *scale-multiplier* 150
  "try modifying me while running!")

(defparameter *sleep-time* 0.0001
  "modify and eval to speed or slow the animation, set to `nil' to stop")

(defparameter *sleep-time* nil)
(defparameter *sleep-time* 0.01)
(defun cos-animation ()
  (let* ((range (loop for k from 0 to (* 2 pi) by 0.1 collect k)) ; length of 62
         (idx 0)
         (record
          (updating-output (*standard-output*)
            (loop for x from (nth idx range) to (+ (nth idx range) (* 2 pi)) by 0.01
               with y-offset = 150
               for x-offset = (- 10 (* *scale-multiplier* (nth idx range)))
               for y-value = (+ y-offset (* *scale-multiplier* (cos x)))
               for x-value = (+ x-offset (* *scale-multiplier* x))
               do (draw-point* *standard-output*
                               x-value
                               y-value
                               :ink +green+
                               :line-thickness 3)))))
    (loop while *sleep-time*
       do (progn (sleep *sleep-time*)
                 (if (= 61 idx) (setq idx 0) (incf idx))
                 (redisplay record *standard-output*)))))

;;; Runme! We will need these in a moment.
(format t "Asd~%")
(dolist (image-name '("mp-avatar.png"
		      "vulpes-avatar.png"
		      "stas-avatar.png"
		      "suit-avatar.png"
		      "rainbow-dash-avatar.png"
		      "chaos-lord-avatar.png"))
  (progn (format t "~a~%" image-name)
	 (uiop:run-program (format nil "curl https://common-lisp.net/project/mcclim/static/media/tutorial-1/~A --output /tmp/~A" 
				   image-name image-name))))



(clim-listener::define-listener-command (com-ls :name t)
  ((path 'string))
  (clim-listener::com-show-directory path))
