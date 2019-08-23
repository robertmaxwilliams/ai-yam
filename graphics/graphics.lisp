(ql:quickload :sdl2)


(print "hehwel,j")

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun all-function-symbols (package)
  "Retrieves all functions in a package."
  (let ((res (list)))
    (do-all-symbols (sym package)
      (when (fboundp sym)
        (push sym res)))
    res))
;;(format t "~a" (all-function-symbols :sdl-2))

(defmacro with-window-surface ((window surface renderer) &body body)
  `(sdl2:with-init 
     (:everything)
     (sdl2:with-window 
       (,window
         :title "SDL2 Tutorial"
         :w *screen-width*
         :h *screen-height*
         :flags '(:shown))
       (sdl2:with-renderer 
         (,renderer window :flags '(:software))
         (let ((,surface (sdl2:get-window-surface ,window)))
           ,@body)))))

(defparameter points  
  '((1 87.951294 2.658162) (2 33.4666 66.682945) (3 91.77831 53.807182) (4 20.526749 47.63329)))

(defun foo (x)
  (floor (* x 4)))

(defparameter colors-alist
  '((black 0 0 0)
    (white 255 255 255)
    (red 255 0 0)
    (green 0 255 0)
    (blue 0 0 255)
    (yellow 255 255 0)
    (cyan 0 255 255)
    (magenta 255 0 255)))

(defun get-color (name)
  (append (cdr (assoc name colors-alist)) '(255)))

;;  Create a simple drawing API
;; (line x1 y1 x2 y2)
;; (square x y width)
;; (rect x y width height)
;; (circle x y radius)
(defmacro truncate-vars (&body vars)
  (cons 'progn 
	(loop for var in vars
	   collect `(setq ,var (truncate ,var)))))


(defun coord-distance (x1 x2 y1 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defun main ()
  (with-window-surface 
    (window surface renderer)
    (sdl2:blit-surface (sdl2:load-bmp "hello_world.bmp")
                       nil
                       surface
                       nil)
    ;;(sdl2:render-draw-line renderer 0 0 100 100)
    (labels ((line (x1 y1 x2 y2 &optional (color 'black))
                   (truncate-vars x1 y2 x2 y2)
                   (apply #'sdl2:set-render-draw-color renderer (get-color color))
                   (sdl2:render-draw-line renderer x1 y1 x2 y2))
             (corner-rect (x1 y1 width height &optional (color 'white))
                          (truncate-vars x1 y1 width height)
                          (apply #'sdl2:set-render-draw-color renderer (get-color color))
                          (sdl2:render-fill-rect renderer (sdl2:make-rect x1 y1 height width)))
             (rect (x y width height &optional (color 'black))
                   (let ((x1 (- x (/ width 2)))
                         (y1 (- y (/ height 2))))
                     (corner-rect x1 y1 width height color)))
             (square (x y width &optional (color 'black))
                     (rect x y width width color))
             (hello (a b)
                    (+ a b))
             (circle (x y radius &optional (color 'black))
                     (truncate-vars x y radius)
                     (apply #'sdl2:set-render-draw-color renderer (get-color color))
                     (loop for i from (- x radius) below (+ x radius)
                           do (loop for j from (- y radius) below (+ y radius)
                                    do (if (<= (coord-distance x y i j) radius)
                                         (sdl2:render-draw-point renderer i j))))))

      (corner-rect 0 0 *screen-width* *screen-height* 'white)
      (rect 100 100 50 50)
      (rect 100 100 10 10 'blue)
      (line 10 10 100 300)
      (line 10 10 100 300 'red)

      (loop for point in points
            do (destructuring-bind (n x y) point
                 (declare (ignore n))
                 (rect x y 30 30)))


      (format t "about to drwa~%")
      (sdl2:update-window window)
      (format t "done drawing, going to loop~%")
      (let ((running t))
        (flet ((close-drawing () (setf running nil)))
          (loop while running
                do (progn
                     (format t "Command: ")
                     (finish-output)
                     (handler-case 
                       (let ((input-ls (read-from-string (read-line))))
                         (format t "Car input: ~a~%" (car input-ls))
                         (format t "~a~%" 
                                 (apply (car input-ls) (cdr input-ls))))
                       (error (c)
                              (format t "Ooops made an error:~%~a~%" c)
                              (values 0 c)))
                     (finish-output)
                     (sdl2:update-window window)))))
      (format t "QUTTTTTING!!!!~%"))))

(handler-case (main)
  (error (c)
    (format t "We caught a condition.~&")
    (sdl2:quit)
    (values 0 c)))

(quit)
