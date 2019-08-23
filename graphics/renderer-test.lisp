
(ql:quickload :sdl2)

(defun test-render-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun test-render-hello (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  ;; H
  (sdl2:render-draw-line renderer 20 20 20 100)
  (sdl2:render-draw-line renderer 20 60 60 60)
  (sdl2:render-draw-line renderer 60 20 60 100)
  ;; E
  (sdl2:render-draw-line renderer 80 20 80 100)
  (sdl2:render-draw-line renderer 80 20 120 20)
  (sdl2:render-draw-line renderer 80 60 120 60)
  (sdl2:render-draw-line renderer 80 100 120 100)
  ;; L
  (sdl2:render-draw-line renderer 140 20 140 100)
  (sdl2:render-draw-line renderer 140 100 180 100)
  ;; L
  (sdl2:render-draw-line renderer 200 20 200 100)
  (sdl2:render-draw-line renderer 200 100 240 100)
  ;; O
  (sdl2:render-draw-line renderer 260 20 260 100)
  (sdl2:render-draw-line renderer 260 100 300 100)
  (sdl2:render-draw-line renderer 300 20 300 100)
  (sdl2:render-draw-line renderer 260 20 300 20))

(defun test-render-lines (renderer)
  (sdl2:with-points ((a 200 200)
                     (b 300 400)
                     (c 400 200))
    (sdl2:set-render-draw-color renderer 0 0 255 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-lines renderer points num))))

(defun test-render-points (renderer)
  (sdl2:with-points ((a (random 800) (random 800))
                     (b (random 800) (random 800))
                     (c (random 800) (random 800)))
    (sdl2:set-render-draw-color renderer 0 255 0 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-points renderer points num))))

(defun test-render-rect (renderer)
  (sdl2:render-draw-rect renderer (sdl2:make-rect 400 400 35 35)))

(defun test-render-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :for y :upto 5
                   :collect (sdl2:make-rect (+ 400 (* x 10)) (+ 200 (* y 10)) 8 8)))
    (sdl2:render-draw-rects renderer rects num)))

(defun test-render-fill-rect (renderer)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 445 400 35 35)))

(defun test-render-fill-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :collect (sdl2:make-rect (+ 500 (* x 10)) 400 8 8)))
    (sdl2:set-render-draw-color renderer 255 0 255 255)
    (sdl2:render-fill-rects renderer rects num)))

(defun renderer-test ()
  "Test the SDL_render.h API"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (test-render-clear renderer)
           (test-render-hello renderer)
           (test-render-lines renderer)
           (test-render-points renderer)
           (test-render-rect renderer)
           (test-render-rects renderer)
           (test-render-fill-rect renderer)
           (test-render-fill-rects renderer)
           (sdl2:render-present renderer)
	   (sdl2:delay 33))
          (:quit () t))))))


(defvar xxx 0)
(defun render-test-2 ()
  "Test the SDL_render.h API"
  (sdl2:init :everything)
  (multiple-value-bind (win renderer) (sdl2:create-window-and-renderer 200 300 '(:shown) )
    (sdl2:with-event-loop 
      (:method :poll)
      (:keydown
        (:keysym keysym)
        (format t "deydown: ~a~%" keysym)
        (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
          (sdl2:push-event :quit)))
      (:idle
        ()
        (incf xxx)
        (test-render-clear renderer)
        (test-render-hello renderer)
        (test-render-lines renderer)
        (test-render-points renderer)
        (test-render-rect renderer)
        (test-render-rects renderer)
        (test-render-fill-rect renderer)
        (test-render-fill-rects renderer)
        (sdl2:render-present renderer)
        (when (> xxx 100)
          (format t "quitting~%")
          (sdl2:push-event :quit))
        (sdl2:delay 33))
      (:quit 
        () 
        (sdl2:destroy-renderer renderer)
        (sdl2:destroy-window win)
        (sdl2:quit)
        (quit)))))

(sb-int:set-floating-point-modes :traps nil)
(render-test-2)

(defun make-two-sdl2-windows ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win1 :title "Win1" :flags '(:shown))
      (sdl2:with-window (win2 :title "Win2" :flags '(:shown))
        (sdl2:with-renderer (ren1 win1 :flags '(:accelerated))
          (sdl2:with-renderer (ren2 win2 :flags '(:accelerated))
             (sdl2:with-event-loop (:method :poll)
               (:keyup
                 (:keysym keysym)
                 (list keysym)
                 (sdl2:push-event :quit))
                 (:idle
                   ()
                   (progn
                     (sdl2:set-render-draw-color ren1 0 0 255 255)
                     (sdl2:set-render-draw-color ren2 0 255 0 255)
                     (sdl2:render-draw-line ren1 150 20 100 300)
                     (sdl2:render-draw-line ren2 20 20 150 150)
                     (sdl2:render-present ren1)
                     (sdl2:render-present ren2)))
                 (:quit () t))))))))
;;(renderer-test)
;;(make-two-sdl2-windows)
