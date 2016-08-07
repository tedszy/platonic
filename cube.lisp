(in-package #:platonic)

;; Rotational symmetries of the cube.

(defparameter *cube-group*
  (mapcar #'make-group-element
	  (list '(1 2 3 4 5 6 7 8) ;; Identity.
		;; Axes through centers of opposite faces. 
		;; There are 3 of them. One, two and three turns each.
		;; Total of 9 elements.
		'(2 3 4 1 6 7 8 5) ;; (1234)--(5678)
		'(3 4 1 2 7 8 5 6)
		'(4 1 2 3 8 5 6 7)
                '(4 3 7 8 1 2 6 5) ;; (1458)--(2673)
		'(8 7 6 5 4 3 2 1)
		'(5 6 2 1 8 7 3 4)
		'(5 1 4 8 6 2 3 7) ;; (1567)--(3478)
		'(6 5 8 7 2 1 4 3)
		'(2 6 7 3 1 5 8 4)
		;; Axes through opposite diagonal points.
		;; There are 4 of them. One and two turns each.
		;; Total of 8 elements.
		'(1 4 8 5 2 3 7 6) ;; (1)--(7)
		'(1 5 6 2 4 8 7 3)
		'(6 2 1 5 7 3 4 8) ;; (2)--(8)	
		'(3 2 6 7 4 1 5 8)
		'(6 7 3 2 5 8 4 1) ;; (3)--(5)		
		'(8 4 3 7 5 1 2 6)
		'(3 7 8 4 2 6 5 1) ;; (4)--(6)
		'(8 5 1 4 7 6 2 3)
		;; Axes through midpoints of diagonally opposing edges.
		;; There are 6 of them. One turn each.
		;; Total of 6 elements. Here g^2 = id.
		'(2 1 5 6 3 4 8 7) ;; (12)--(78)
		'(7 3 2 6 8 4 1 5) ;; (23)--(58)
		'(7 8 4 3 6 5 1 2) ;; (34)--(56) 
		'(5 8 7 6 1 4 3 2) ;; (15)--(37)
		'(4 8 5 1 3 7 6 2) ;; (14)--(67)
		'(7 6 5 8 3 2 1 4) ;; (48)--(26)
		)))

(defun checkc ()
  (let ((res nil))
    (loop for g in *cube-group*
       do (loop for h in *cube-group*
	     do 
	       (push (g* h g) res)
	       (push (g* g h g) res)
	       (push (g* h g h) res)
	       (push (g* h h g) res)
	       (push (g* g g g h) res)))
    (values (length res)
	    (length (remove-duplicates res :test #'equalp)))))

