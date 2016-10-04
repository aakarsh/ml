(defun ml:sign-pred(p)
  (if p 1 -1))

(defun ml:sign(x)
  (ml:sign-pred (>= x 0)))

(defun ml:flip(a b)
  "Return a,b with equal probability"
  (if (>= 0.5 (random* 1.0)) a b))

(defun ml:gen-value()
  "Generates a floating point value in the interval of -1 to 1"
  (let* ((sign (ml:flip -1 1)))
    (* sign (random* 1.0))))

(defun ml:gen-point()
  "Generate a point (x y) in the interval -1 to 1"
  (list (ml:gen-value) (ml:gen-value)))

(defun ml:gen-data-set(n)
  "Generate a dataset of `n` points in the interval of -1 to 1 "
  (let ((ls '()))
    (dotimes (i n ls)
        (push (ml:gen-point) ls))
    ls))

(defun ml:determinant(m)
  "Determinent of 3x3 matrix"
  (flet ((r (v x y) (aref (aref v x) y)))
    (-
     (+ (* (r m 0 0) (r m 1 1) (r m 2 2))
        (* (r m 0 1) (r m 1 2) (r m 2 0))
        (* (r m 0 2) (r m 2 0) (r m 2 1)))
     (+ (* (r m 0 0) (r m 2 1) (r m 1 2))
        (* (r m 0 1) (r m 1 0) (r m 2 2))
        (* (r m 0 2) (r m 2 0) (r m 1 1))))))

(defun ml:classify-point (p1 p2 x)
  "Classifies point x as to the left or right of vector p1->p2 "
  (let ((p1 p1)
        (p2 p2)
        (tmp nil))        
    (if (> (car p1) (car p2))
        (progn
          (setq tmp p1)
          (setq p1 p2)
          (setq p2 tmp)))          
  (ml:sign
   (ml:determinant
    (vector
     (vector (car p1) (cadr p1) 1)
     (vector (car p2) (cadr p2) 1)
     (vector (car x)   (cadr x) 1))))))

(defun ml:classify-by-w (w bias point)
  (ml:sign-pred
   (> (+ bias
         (* (car point) (aref  w 0))
         (* (cadr point) (aref  w 1))) 0)))

(defun ml:get-target-function(p1 p2)
  "Return function to classify point."
    (lambda(point)
      (ml:classify-point p1 p1 point)))

(defun ml:gen-target-function()
  "Generates a random classification function"
  (ml:get-target-function (ml:gen-point) (ml:gen-point)))

(defun ml:update-weights (classification point weights))

(defun ml:pla(points p1 p2 niter)
  (let ((weights  (vector 0 0))
        (bias      0)
        (classification-errors 0))
    (dotimes (n  niter)
      (setq classification-errors 0)
      (dolist (p points)
        (let ((classification
               (ml:classify-point  p1 p2 p)))
          (if (not (eq classification
                     (ml:classify-by-w weights bias p)))
              (progn
                ;; update the bias
                (incf classification-errors)
                (setq bias (+ classification bias))
                (aset weights 0 (+ (aref weights 0)
                                   (* classification (car p))))
                (aset weights 1 (+ (aref weights 1)
                                   (* classification (cadr p)))))))))
    (list weights bias classification-errors)))

(setq point1 (ml:gen-point))
(setq point2 (ml:gen-point))
(setq ds1    (ml:gen-data-set 100))

(provide 'ml)