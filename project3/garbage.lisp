
(defun angle-a (a b c)
  "find angle at A"
  (acos 
   (/ (+ (sqr b) (sqr c) (- (sqr a)))
      (* 2 b c))))
(defun distance-angles (d0 d1 d2)
  " finds the angles in the corner "
  (list
   (angle-a d0 d1 d2)
   (angle-a d1 d2 d0)
   (angle-a d2 d0 d1)))

(defun points-to-distances (p0 p1 p2)
  (mapcar #'helpers:distance
	  (list p1 p2 p0)
	  (list p2 p0 p1)))

(defun angles-from-points (p0 p1 p2)
  (apply #'distance-angles 
	 (points-to-distances p0 p1 p2)))
(defun rad->deg (rad)
  (* rad (/ 180 pi)))

(mapcar #'rad->deg
	(angles-from-points '(0 6 0.2) '(1 2 5) '(2 1.1 2.2)))

(defun distance-to-line (p0 p1 p2)
  " where p0 is the point off the line"
  (destructuring-bind ((n0 x0 y0) (n1 x1 y1) (n2 x2 y2))
      (list p0 p1 p2)
    (declare (ignore n0 n1 n2))
    (/ (abs (+
	     (- (* (- y2 y1) x0) (* (- x1 x2) y0))
	     (- (* x2 y1) (* y2 x1))))
       (sqrt (+
	      (sqr (- y2 y1))
	      (sqr (- x2 x1)))))))

(defun distance-to-line-segment (p0 p1 p2)
  (destructuring-bind (a0 a1 a2) (angles-from-points p0 p1 p2)
    (declare (ignore a0))
    (cond
      ((>= a1 (/ pi 2))
       (format *s* "(closest-is-point ~a ~a)~%" p0 p1)
	(helpers:distance p0 p1))
      ((>= a2 (/ pi 2))
       (format *s* "(closest-is-point ~a ~a)~%" p0 p2)
       (helpers:distance p0 p2))
      (t
       (format *s* "(closest-is-line ~a ~a ~a)~%" p0 p1 p2)
       (distance-to-line p0 p1 p2)))))
