

(ql:quickload :bt-semaphore)
(defparameter *counter* 0)

(defun test-update-global-variable ()
  (bt:make-thread
   (lambda ()
     (loop 
	do (sleep 1)
	do (incf *counter*))))
  *counter*)
(test-update-global-variable)
(identity *counter*)
