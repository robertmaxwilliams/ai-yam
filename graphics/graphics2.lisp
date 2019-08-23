(ql:register-local-projects) 
(ql:quickload 'mcclim)
(ql:quickload 'clim-listener)
(clim-listener:run-listener :new-process t)

(in-package climi)

(defparameter *scale-multiplier* 150
  "try modifying me while running!")

(defparameter *sleep-time* 0.0001
  "modify and eval to speed or slow the animation, set to `nil' to stop")

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
(cos-animation)
