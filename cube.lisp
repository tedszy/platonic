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

		;; problem here.

                '(4 3 7 8 1 2 6 5) ;; (1458)--(2367)
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

(defun make-cube-configuration (vef-type data)
  (let ((vertices '((1) (2) (3) (4) (5) (6) (7) (8)))
	(edges '((1 2) (1 4) (1 5) (2 3) (2 6) (3 4) (3 7) (4 8) (5 6) (5 8) (6 7) (7 8)))
	(faces '((1 2 3 4) (1 2 5 6) (1 4 5 8) (2 3 6 7) (3 4 7 8) (5 6 7 8))))
    (cons vef-type 
	  (ecase vef-type
	    (vertices (loop for v in vertices and d in data
			 collecting (list v d)))
	    (edges (loop for e in edges and d in data
		      collecting (list e d)))
	    (faces (loop for f in faces and d in data
		      collecting (list f d)))))))

(defun all-cube-colorings (colors)
  (let ((color-lists (make-list 6 :initial-element colors)))
    (mapcar #'(lambda (c) (make-cube-configuration 'faces c))
	    (apply #'cartesian-product color-lists))))
				  
(defun distinct-cube-colorings (colors)
  (let ((all (all-cube-colorings colors))
	(result nil))
    (loop until (null all)
       with current = nil
       with rotations = nil
       do
	 (setf current (pop all))
	 (setf rotations (loop for g in *cube-group*
			    collecting (transform-configuration g current)))
	 (setf all (set-difference all rotations :test #'equalp))
	 (push current result))
    result))



