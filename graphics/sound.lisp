
(ql:quickload :uiop) ;;uses local install because the ql repo is bad
(defparameter *dsp* (uiop:launch-program "aplay" :input :stream :output :stream))

(progn
  (loop for i from 0 to 8000
     do(write-byte (floor (* 100 (1+ (sin (* i 0.9)))))
		   (uiop:process-info-input *dsp*)))
  (force-output (uiop:process-info-input *dsp*)))

(uiop:close-streams *dsp*)
