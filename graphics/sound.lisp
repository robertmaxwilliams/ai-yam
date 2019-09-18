(ql:quickload :iterate)
(ql:quickload :alexandria)

(load "graphics.lisp")

(defpackage :music-machine
  (:use :cl :iterate :alexandria :max-graphics))
(in-package :music-machine)

(ql:quickload :uiop) ;;uses local install because the ql repo is bad

(close-audio)
(let ((aplay-command "aplay -f U8 -c1 --rate 44100 --buffer-size 128"))
  (defparameter *dsp* (uiop:launch-program aplay-command :input :stream :output :stream)))

(defun close-audio ()
  (uiop:close-streams *dsp*))

(defparameter *sps* 44100.0)
(defparameter *bs* 128)

(defparameter *freq* 440.0)
(defparameter *sounds* nil)
(defparameter *quit* nil)
(defparameter *quit* t)

(defun tone-maker (freq secs &optional (volume 0.01))
  (let ((i 0))
    (lambda ()
      (incf i)
      (if (> i (* secs *sps*))
	  nil
	  (* volume (sin (* 2 pi i (/ freq *sps*))))))))

(defun frob-to-byte (frob)
  (floor (* 100 (1+ frob))))

(defun update-sounds ()
  (let ((new-sounds nil)
	(frob 0.0))
    (iter (for s in *sounds*)
	  (for x = (funcall s))
	  (when x
	    (incf frob x)
	    (push s new-sounds)))
    (setq *sounds* new-sounds)
    frob))


(defun frob-to-two-byte (frob)
  (the (mod 65538) (nth-value 0 (floor (* 65536 (/ (1+ frob) 2))))))

(defun write-two-byte (two-byte stream)
  (declare ((mod 65536) two-byte))
  (write-byte (ldb (byte 8 8) two-byte) stream)
  (write-byte (ldb (byte 8 0) two-byte) stream))
	   

(defun run-sound-eating-loop ()
  (let* ((aplay-command "aplay -f S16_LE -c1 --rate 44100 --buffer-size 64")
	 (dsp (uiop:launch-program aplay-command :input :stream :output :stream))
	 (millis-per-block (* 1000 *bs* (/ 1 *sps*)))
	 (start (get-internal-real-time)))
    (iter (for i from 0)
	  (if *quit* (return))
	  (iter (repeat *bs*)
		(write-two-byte (frob-to-two-byte (update-sounds)) (uiop:process-info-input dsp)))
	  (force-output (uiop:process-info-input dsp))
	  ;;(format t "dt: ~A~%" (- (+ start (* i millis-per-block)) (get-internal-real-time)))
	  (when (> (- (+ start (* i millis-per-block)) (get-internal-real-time)) 100)
	    ;;(format t "got ahead, sleeping ~a~%" i)
	    (sleep (/ 40 1000))))
    (force-output (uiop:process-info-input dsp))
    (uiop:close-streams dsp)
    (format t "Done.~%")))
(defun kill-sound-eating-loop ()
  (progn (defparameter *quit* t)  (sleep 0.2) (defparameter *quit* nil)))


(defparameter foobar (tone-maker 440.0 10 0.00002))
(frob-to-two-byte (funcall foobar))

(defun play-note (freq duration &optional (volume 0.0007))
  (push (tone-maker (* 440 freq) duration volume) *sounds*))
(defun play-chord (freqs duration &optional (volume 0.0007))
  (iter (for f in freqs)
	(play-note f duration (/ volume (length freqs)))))
(play-note 1 1)
(play-chord (list 1 3/5 7/3) 1)

(run-sound-eating-loop)
(progn
  (push (tone-maker (* 3/2 440.0) 2 0.0007) *sounds*)
  (push (tone-maker 440.0 2 0.0007) *sounds*))
(setq *sounds* nil)
(push (tone-maker 441.0 1.5) *sounds*)
(main)
(kill-sound-eating-loop)

(defun render ()
  (gl:clear :color-buffer)
  (draw-circle 50 50 4 :white)
  (draw-arrow-with-offsets 50 50 70 70 2 5 4 4 :blue)
  (draw-circle 70 70 4 :white)
  (draw-number 6 70 70 4 :black)
  (gl:flush))

(defun @@ (fun-plist keyword)
    (getf fun-plist keyword))
(defun @ (fun-plist keyword &rest args)
    (apply (getf fun-plist keyword) args))

(defun sine1 (x)
  "sine with period 1"
  (sin (* x 2 pi)))

(defun triangle (x)
  "saw from -1 to 1 with period 1"
  (1- (* 4 (abs (- (mod x 1) 0.5)))))

(loop for i from 0 to 50
     collect (saw (/ i 50)))
  

(defun toner-maker (init-freq init-volume)
  (let ((numerator 1)
	(denominator 1)
	(base-freq init-freq) 
	(i 0)
	(is-running t)
	(volume init-volume))
    (list
     :main
     (lambda ()
       (cond (is-running
	      (incf i)
	      (let* ((freq (* base-freq (/ numerator denominator)))
		     (adj (/ base-freq freq)))
	      (* adj volume (triangle (* i (/ freq *sps*))))))
	     (t 0)))
     :set-numerator (lambda (n) (setq numerator n))
     :set-denominator (lambda (d) (setq denominator d))
     :get-numerator  (lambda () numerator)
     :get-denominator  (lambda () denominator)
     :multiply-freq (lambda (multiplier) (setq base-freq (* base-freq multiplier)))
     :set-running (lambda (x) (setq is-running x))
     :multiply-volume (lambda (multiplier) (setq volume (* volume multiplier))))))

(defun scale (n)
  (expt 2 (/ n 12)))

(main) 

(defun main-loop (win render-fn)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (declare (ignorable render-fn))
  (let ((t1 (toner-maker 440.0 0.0001))
	(t2 (toner-maker 220.0 0.0003)))
    (setq *sounds* nil)
    (@ t2 :multiply-volume 2)
    (push (@@ t1 :main) *sounds*)
    (push (@@ t2 :main) *sounds*)
    (sdl2:with-event-loop (:method :poll)
      (:idle
       ()
       ;;(funcall render-fn)
       (gl:clear :color-buffer)
       (draw-number (@ t1 :get-numerator) 70 80 4 :green)
       (draw-number (@ t1 :get-denominator) 70 70 4 :green)
       (draw-number (@ t2 :get-numerator) 80 80 4 :green)
       (draw-number (@ t2 :get-denominator) 80 70 4 :green)
       (gl:flush)
       ;; Swap back buffer
       (sdl2:gl-swap-window win)
       (sdl2:delay 33))
      (:keydown
       (:keysym keysym)
       (format t "Key pressed: ~a~%" (sdl2:scancode keysym))
       (case (sdl2:scancode keysym)

	 (:scancode-1 (@ t1 :set-denominator 1)) 
	 (:scancode-q (@ t1 :set-denominator 2)) 
	 (:scancode-a (@ t1 :set-denominator 3)) 
	 (:scancode-z (@ t1 :set-denominator 5)) 

	 (:scancode-2 (@ t1 :set-numerator 1)) 
	 (:scancode-w (@ t1 :set-numerator 2)) 
	 (:scancode-s (@ t1 :set-numerator 3)) 
	 (:scancode-x (@ t1 :set-numerator 5))

	 (:scancode-3 (@ t2 :set-denominator 1)) 
	 (:scancode-e (@ t2 :set-denominator 2)) 
	 (:scancode-d (@ t2 :set-denominator 3)) 
	 (:scancode-c (@ t2 :set-denominator 5)) 

	 (:scancode-4 (@ t2 :set-numerator 1)) 
	 (:scancode-r (@ t2 :set-numerator 2)) 
	 (:scancode-f (@ t2 :set-numerator 3)) 
	 (:scancode-v (@ t2 :set-numerator 5))



	 (:scancode-escape (sdl2:push-event :quit))))
      (:keyup
       (:keysym keysym)
       (format t "Key unpressed: ~a~%" (sdl2:scancode keysym))
       (case (sdl2:scancode keysym)
	 (:scancode-a nil)))
      (:quit () t)))
  (setq *sounds* nil))

(main)
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
        (setup-gl win gl-context)
        (main-loop win #'render)))))

(main)

"
       (:scancode-a (push (tone-maker (* 440.0 (expt 2 (/ 0 12))) 0.1) *sounds*))
       (:scancode-s (push (tone-maker (* 440.0 (expt 2 (/ 2 12))) 0.1) *sounds*))
       (:scancode-d (push (tone-maker (* 440.0 (expt 2 (/ 4 12))) 0.1) *sounds*))
       (:scancode-f (push (tone-maker (* 440.0 (expt 2 (/ 5 12))) 0.1) *sounds*))
       (:scancode-g (push (tone-maker (* 440.0 (expt 2 (/ 7 12))) 0.1) *sounds*))
       (:scancode-h (push (tone-maker (* 440.0 (expt 2 (/ 9 12))) 0.1) *sounds*))
       (:scancode-j (push (tone-maker (* 440.0 (expt 2 (/ 11 12))) 0.1) *sounds*))
       (:scancode-k (push (tone-maker (* 440.0 (expt 2 (/ 12 12))) 0.1) *sounds*))
       (:scancode-q (push (tone-maker (* 440.0 (expt 2 (/ 0 12))) 0.1) *sounds*))
       (:scancode-w (push (tone-maker (* 440.0 (expt 2 (/ 2 12))) 0.1) *sounds*))
       (:scancode-e (push (tone-maker (* 440.0 (expt 2 (/ 3 12))) 0.1) *sounds*))
       (:scancode-r (push (tone-maker (* 440.0 (expt 2 (/ 5 12))) 0.1) *sounds*))
       (:scancode-t (push (tone-maker (* 440.0 (expt 2 (/ 7 12))) 0.1) *sounds*))
       (:scancode-y (push (tone-maker (* 440.0 (expt 2 (/ 8 12))) 0.1) *sounds*))
       (:scancode-u (push (tone-maker (* 440.0 (expt 2 (/ 10 12))) 0.1) *sounds*))
       (:scancode-i (push (tone-maker (* 440.0 (expt 2 (/ 12 12))) 0.1) *sounds*))



	 (:scancode-2 (funcall (cadddr t1) 3/2))
	 (:scancode-1 (funcall (cadddr t1) 2/3))
	 (:scancode-w (funcall (caddr t1) t))
	 (:scancode-q (funcall (caddr t1) nil))
	 (:scancode-s (funcall (cadr t1) (scale 1)))
	 (:scancode-a (funcall (cadr t1) (scale -1)))
	 (:scancode-x (funcall (cadr t1) (scale 2)))
	 (:scancode-z (funcall (cadr t1) (scale -2)))

	 (:scancode-4 (funcall (cadddr t1) 3/2))
	 (:scancode-3 (funcall (cadddr t1) 2/3))
	 (:scancode-r (funcall (caddr t2) t))
	 (:scancode-e (funcall (caddr t2) nil))
	 (:scancode-f (funcall (cadr t2) (scale 1)))
	 (:scancode-d (funcall (cadr t2) (scale -1)))
	 (:scancode-v (funcall (cadr t2) (scale 2)))
	 (:scancode-c (funcall (cadr t2) (scale -2)))

"
