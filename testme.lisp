(in-package #:platonic)
		
;; Burnside's formulas for tetrahedron and cube.
;; c = number of colors.	

(defun burnside-tetrahedron (c)
  (/ (+ (* 11 (* c c))
	(* c c c c))
     12))

(defun burnside-cube (c)
  (* (/ 24)
     (+ (* c c c c c c)
	(* 3 c c c c)
	(* 12 c c c)
	(* 8 c c))))

(init-testing)

(in-group 'groups)

(test group-elements (make-group-element '(1 2 3 4)) equalp  #(0 1 2 3 4))
(let ((s (make-group-element '(4 3 2 1)))
      (r (make-group-element '(1 3 4 2))))
  (test group-elements (g* s s) equalp #(0 1 2 3 4)) 
  (test group-elements (g* s r) equalp #(0 4 2 1 3))
  (test group-elements (g* r s) equalp #(0 2 4 3 1)))

(test tetrahedron-identity (has-identity-property-p *tetrahedron-group*) eq t)
(test tetrahedron-inverses (has-inverse-property-p *tetrahedron-group*) eq t)
(test tetrahedron-closure  (has-closure-property-p *tetrahedron-group*) eq t)

(test cube-identity (has-identity-property-p *cube-group*) eq t)
(test cube-inverses (has-inverse-property-p *cube-group*) eq t)
(test cube-closure (has-closure-property-p *cube-group*) eq t)

(in-group 'geometry)

(test vertex/edge/face-hash (hash-vertex/edge/face '(6 2 3 5)) = 2356)
(let ((id (make-group-element '(1 2 3 4))))
  (test vertex/edge/face-transform (transform-vertex/edge/face id '(1 2 3)) equalp '(1 2 3)))

(let ((c (make-tetrahedron-configuration 'faces '(a b c d)))
      (g (make-group-element '(4 3 2 1)))
      (h (make-group-element '(3 1 4 2))))
  (test configuration 
	(transform-configuration g c) 
	equalp 
	'(faces ((1 2 3) D) ((1 2 4) C) ((1 3 4) B) ((2 3 4) A)))
  (test configuration
	(transform-configuration h c)
	equalp
	'(faces ((1 2 3) B) ((1 2 4) D) ((1 3 4) A) ((2 3 4) C))))

(in-group 'tetrahedron-and-cube)

(test tetrahedron-2 (length (distinct-tetrahedron-colorings '(r g))) = (burnside-tetrahedron 2))
(test tetrahedron-3 (length (distinct-tetrahedron-colorings '(r g b))) = (burnside-tetrahedron 3))
(test tetrahedron-4 (length (distinct-tetrahedron-colorings '(r g b y))) = (burnside-tetrahedron 4))
(test tetrahedron-5 (length (distinct-tetrahedron-colorings '(r g b y k))) = (burnside-tetrahedron 5))

(test cube-2 (length (distinct-cube-colorings '(r g))) = (burnside-cube 2))
(test cube-3 (length (distinct-cube-colorings '(r g b))) = (burnside-cube 3))


