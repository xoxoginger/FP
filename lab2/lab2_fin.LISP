(defun delete_end(list)                      
  (cond ((null (cdr list)) '())                        
        (t(cons (car list) (delete_end (cdr list))))))
		  
(defun sum12 (list1)
  (+ (nth 0 list1) (nth 1 list1)))

(defun mult2 (list2)
  (* 2 (nth ( - (length list2) 1) list2)))

(defun mult (list1 list2)
  (cond ( (or (<= (length list1) 2) (<= (length list2) 2)) 1)
        ((* (+ (sum12 list1) (mult2 list2)) (fmain (rest list1) (delete_end list2))))))
	
(defun product-sum3 (lst)
  (let ((x (mult lst lst))) (cond ((= x 1) "wrong list, try another") (x))))
