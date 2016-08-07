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

(defun cartesian-product (&rest lists)
  (if (car lists)
      (mapcan (lambda (inner)
                (mapcar #'(lambda (outer) (cons outer inner))
                        (car lists)))
              (apply #'cartesian-product (cdr lists)))
      (list nil)))
