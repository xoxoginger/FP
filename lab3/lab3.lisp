(defun print-matrix (matrix &optional (chars 3) stream)
    (let ((*print-right-margin* (+ 6 (* (1+ chars) 
                                        (array-dimension matrix 1)))))
        (pprint matrix stream)
        (values)))

(defun swap-rows (mtrx r1 r2)
    (loop with n = (array-dimension mtrx 1)
        for i upfrom 0 below n do
            (rotatef (aref mtrx r1 i) (aref mtrx r2 i)))
    mtrx)

(defun swap-cols (mtrx c1 c2)
    (loop with n = (array-dimension mtrx 0)
        for i upfrom 0 below n do
            (rotatef (aref mtrx i c1) (aref mtrx i c2)))
    mtrx)

(defun pos-of-min (mtrx)
    (let ((min-row 0) (min-column 0) (min-elem (aref mtrx 0 0)))
        (loop with n = (array-dimension mtrx 0)
            for i upfrom 0 below n do
            (loop with m = (array-dimension mtrx 1) 
            for j upfrom 0 below m do
                (cond ((< (aref mtrx i j) min-elem) 
                    (setf min-elem (aref mtrx i j) min-row i min-column j)))))
        (values min-row min-column)))

(defun copy-array (array) 
    (adjust-array array (array-dimensions array)))

(defun swap-min-to-bottom-left (mtrx)
    (let ((new-mtrx (copy-array mtrx)))
        (multiple-value-bind (i j) (pos-of-min mtrx) 
            (swap-rows new-mtrx (- (array-dimension mtrx 1) 1) i)
            (swap-cols new-mtrx 0 j))
        new-mtrx))


(defvar *m* (make-array '(4 4) :initial-contents '((1 2 3 4) 
                                                    (5 -6 7 8) 
                                                    (9 0 9 2)
                                                    (3 4 5 6))))
(defvar *n* NIL)

(defvar *x* (make-array '(5 5) :initial-contents '((5 7 -9 13 6) 
                                                    (56 8 -1 2 4) 
                                                    (8 1 6 77 9)
                                                    (1 4 -2 16 -32)
                                                    (5 3 2 5 9))))
(defvar *y* NIL)


 

