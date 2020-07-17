(defclass cart ()                
 ((x :initarg :x :reader cart-x)  
  (y :initarg :y :reader cart-y))) 

(defclass polar ()
 ((radius :initarg :radius :accessor radius) 	
  (angle  :initarg :angle  :accessor angle)))

(defmethod print-object ((c cart) stream)
  (format stream "[CART x ~d y ~d]"(cart-x c) (cart-y c) ))

(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))

(defclass line()
	(
		(start :initarg :start :accessor line-start)
		(end   :initarg :end   :accessor line-end)
	)
)

(defmethod print-object((lin line) stream)
	(format stream "[LINE ~s ~s]"
		(line-start lin) (line-end lin)
	)
)

(defmethod cart-x ((p polar))
  (* (radius p) (cos (angle p))))

(defmethod cart-y ((p polar))
  (* (radius p) (sin (angle p))))


(defun pow2 (x) (* x x))

(defmethod calc-dist ((c1 cart) (c2 cart))
    (sqrt2 (+ (pow2 (abs (- (cart-x c2) (cart-x c1))) ) (pow2 (abs (- (cart-y c2) (cart-y c1))))))
    ) 

(defun line-length(lin)
  (setq dot1 (to-cart (line-start lin)))
  (calc-dist dot1 (line-end lin))
)

(setq lin (make-instance 'line
	:start (make-instance 'polar :radius 5 :angle (/ pi 2))
	:end (make-instance 'cart :x 0 :y 0))
)

(line-length lin)


(setq lin (make-instance 'line
	:start (make-instance 'polar :radius 10 :angle (/ pi 4))
	:end (make-instance 'cart :x 4 :y -7))
)

(line-length lin)
