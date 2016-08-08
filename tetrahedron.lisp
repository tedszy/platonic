(in-package #:platonic)

;; Rotational symmetries of the tetrahedron.

(defparameter *tetrahedron-group*
  (mapcar #'make-group-element
	  (list '(1 2 3 4) ;; Identity.
		;; Axes rough vertex and center of opposite face.
		;; There are 4 such axes.
		;; One and two turns around axis. 
		;; Total of 8 elements.
		'(1 3 4 2) 
		'(1 4 2 3)
		'(4 2 1 3)
		'(3 2 4 1)
		'(4 1 3 2)
		'(2 4 3 1)
		'(2 3 1 4)
		'(3 1 2 4)
		;; Axes through midpoints of corresponding opposite edges.
		;; There are three such axes.
		;; One turn. Total of 3 elements
		'(4 3 2 1)
		'(2 1 4 3)
		'(3 4 1 2)
		)))

(defun make-tetrahedron-configuration (vef-type data)
  (let ((vertices '((1) (2) (3) (4)))
	(edges '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
	(faces '((1 2 3) (1 2 4) (1 3 4) (2 3 4))))
    (cons vef-type 
	  (ecase vef-type
	    (vertices (loop for v in vertices and d in data
			 collecting (list v d)))
	    (edges (loop for e in edges and d in data
		      collecting (list e d)))
	    (faces (loop for f in faces and d in data
		      collecting (list f d)))))))

(defun all-tetrahedron-colorings (colors)
  (let ((color-lists (make-list 4 :initial-element colors)))
    (mapcar #'(lambda (c) (make-tetrahedron-configuration 'faces c))
	    (apply #'cartesian-product color-lists))))
				  
(defun distinct-tetrahedron-colorings (colors)
  (let ((all (all-tetrahedron-colorings colors))
	(result nil))
    (loop until (null all)
       with current = nil
       with rotations = nil
       do
	 (setf current (pop all))
	 (setf rotations 
	       (loop for g in *tetrahedron-group*
		  collecting (transform-configuration g current)))
	 (setf all (set-difference all rotations :test #'equalp))
	 (push current result))
    result))
