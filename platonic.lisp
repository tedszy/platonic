(in-package #:platonic)

;; Group elements are represented by permutations in the
;; form of (n+1)-length vectors. The 0th component of the 
;; vector us unused for now. This is to make permutations 
;; begin at index 1 like in all textbooks.

(defun make-group-element (permutation-list)
  (make-array (1+ (length permutation-list)) 
	      :initial-contents (cons 0 permutation-list)))
			   
;; (1 2 3 4)         (1 2 3 4)         (1 2 3 4) 
;; (4 3 2 1) => g    (2 1 4 3) => h    (3 4 1 2) => g*h
(defun group-element-multiply (g h)
  (loop with g*h = (make-array (length g) :initial-element 0)
       for i from 1 below (length g)
       do (setf (svref g*h i) (svref g (svref h i)))
       finally (return g*h)))

;; Convenience.
(defun g* (&rest elements)
  (reduce #'group-element-multiply elements))

;; Geometric objects: vertices, edges and faces 
;; are just sets (i.e. unordered) represented as lists. 
;; We can call these vefs for short (vertex/edge/face).
;;
;; '(1) => vertex
;; '(1 2) => edge
;; '(1 2 3) => face.

;; Hashing function to put vertices, faces, edges into standard order.
;; (a b c d e)
;;  0 1 2 3 4
;;
;; => a*1 + b*10 + c*10^2 + d*10^3 + e*10^4.
(defun hash-vef (vef)
  (loop for u in (sort vef #'<)
       with multiplier = 1
       summing (* u multiplier)
       do (setf multiplier (* 10 multiplier))))
       
;; Transform geometric vef by applying group-element.
(defun transform-vef (g vef)
  (loop for v in vef collecting (svref g v)))

;; Configurations are lists of vertices/edges/faces with extra 
;; data added, such as color of each face. The configuration
;; is represented as an alist.
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

;; Transforms a configuration and then brings it 
;; into a standard sorted form. Each vef is sorted,
;; and the entire configuration list is sorted.
(defun transform-configuration (g config)
  (cons (car config)
	(sort (loop for c in (cdr config)
		 collecting (list (sort (transform-vef g (car c)) #'<)
				  (cadr c)))
	      #'<
	      :key #'(lambda (c) (hash-vef (car c))))))

;; Rotational symmetries of the tetrahedron.

(defparameter *tetrahedral-group*
  (mapcar #'make-group-element
	  (list '(1 2 3 4) ;; Identity.
		;; Axes rough vertex and center of opposite face.
		;; There are 4 such axes.
		;; One and two turns around axis. Total of 12 elements.
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
		;; One turn.
		'(4 3 2 1)
		'(2 1 4 3)
		'(3 4 1 2)
		)))

;; Rotational symmetries of the cube.

(defparameter *cube-group*
  (mapcar #'make-group-element
	  (list '(1 2 3 4 5 6 7 8) ;; Identity.

		;; Axes through centers of opposite faces. 
		;; There are 3 of them. One, two and three turns each.
		;; Total of 9 elements.



		;; Axes through opposite diagonal points.
		;; There are 4 of them. One and two turns each.
		;; Total of 8 elements.



		;; Axes through midpoints of diagonally opposing edges.
		;; There are 6 of them. One turn each.
		;; Total of 6 elements.
		


		)))



;; Find all distinct configurations.
;; Give it a list of colors. See if it matches
;; Burnside's formula.

(defun cartesian-product (&rest lists)
  (if (car lists)
      (mapcan (lambda (inner)
                (mapcar #'(lambda (outer) (cons outer inner))
                        (car lists)))
              (apply #'cartesian-product (cdr lists)))
      (list nil)))

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
	 (setf rotations (loop for g in *tetrahedral-group*
			    collecting (transform-configuration g current)))
	 (setf all (set-difference all rotations :test #'equalp))
	 (push current result))
    result))
		
;; Burnside's formula for tetrahedron.
;; c = number of colors.	
(defun burnside (c)
  (/ (+ (* 11 (* c c))
	(* c c c c))
     12))
	   
;; ---------------------- Tests. --------------------------- 

(defparameter *passed* 0)
(defparameter *failed* 0)

(defmacro test-me (compare expected (fn &rest body))
  `(if (funcall #',compare ,expected (,fn ,@body))
       (progn (incf *passed*) 
	      (format t "ok: ~a~%" ',fn))
       (progn (incf *failed*)
	      (format t "not ok: ~a got=~a, expected=~a~%"
		      ',fn
		      (,fn ,@body)
		      ,expected))))

(defun test-group-elements ()
  (test-me equalp #(0 1 2 3 4) (make-group-element '(1 2 3 4)))
  (let ((s (make-group-element '(4 3 2 1)))
	(r (make-group-element '(1 3 4 2))))
    (test-me equalp #(0 1 2 3 4) (g* s s))
    (test-me equalp #(0 4 2 1 3) (g* s r))
    (test-me equalp #(0 2 4 3 1) (g* r s)))
  )

(defun test-vef ()
  (test-me = 6532 (hash-vef '(6 2 3 5)))
  (let ((id (make-group-element '(1 2 3 4))))
    (test-me equalp '(1 2 3) (transform-vef id '(1 2 3))))
  )

;; Take advantage of the fact that faces are transformed
;; just like vertices for a tetrahedron.
(defun test-configuration ()
  (let ((c (make-tetrahedron-configuration 'faces '(a b c d)))
	(g (make-group-element '(4 3 2 1)))
	(h (make-group-element '(3 1 4 2))))
    (test-me equalp 
	     '(faces ((1 2 3) D) ((1 2 4) C) ((1 3 4) B) ((2 3 4) A))
	     (transform-configuration g c))
    (test-me equalp
	     '(faces ((1 2 3) B) ((1 2 4) D) ((1 3 4) A) ((2 3 4) C))
	     (transform-configuration h c))))

(defun test-burnside ()
  (test-me = 5 (length (distinct-tetrahedron-colorings '(r g))))
  (test-me = 15 (length (distinct-tetrahedron-colorings '(r g b))))
  (test-me = 36 (length (distinct-tetrahedron-colorings '(r g b y))))
  (test-me = 75 (length (distinct-tetrahedron-colorings '(r g b y k)))))

(defun run-tests ()
  (setf *passed* 0)
  (setf *failed* 0)
  (test-group-elements)
  (test-vef)
  (test-configuration)
  (test-burnside)
  (format t "~&passed: ~a" *passed*)
  (format t "~&failed: ~a" *failed*))

(run-tests)



