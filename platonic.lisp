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
	(edges '((1 2) (1 3) (1 4) (2 3) (3 4) (2 4)))
	(faces '((1 2 3) (1 3 4) (1 2 4) (2 3 4))))
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
    
(defparameter c1 (make-tetrahedron-configuration 'faces '(a b c d)))
(defparameter gg (make-group-element '(4 3 2 1)))




;; ----- Tetrahedron data. -----

(defparameter *tetrahedron-vertices* '((1) (2) (3) (4)))
(defparameter *tetrahedron-edges* '((1 2) (1 3) (1 4) (2 3) (3 4) (2 4)))
(defparameter *tetrahedron-faces* '((1 2 3) (1 3 4) (1 2 4) (2 3 4)))

;; Define group of tetrahedral rotational symmetries. 
;; Two generators r, s.
;; Standard vertex-edge-face labelling (see graphic).
;; r => 120 degree twist through axis on vertex1 and base 1.
;; s => 180 degree twist through axis on midpoints of edge 14 and 23.
(defparameter *tetrahedral-group*
  (let ((s (make-group-element '(4 3 2 1)))
	(r (make-group-element '(1 3 4 2))))
    (list s r 
	  (g* s s) (g* r r) (g* s r) (g* r s)
	  (g* r s r) (g* r r s) (g* s r r)
	  (g* r r s r) (g* r s r r))))

;; -----------------------------





;; ---------------------- testing --------------------------- 

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

(defun run-tests ()
  (setf *passed* 0)
  (setf *failed* 0)
  (test-group-elements)
  (test-vef)
  (format t "~&passed: ~a" *passed*)
  (format t "~&failed: ~a" *failed*))

(run-tests)


;; ------------------------------------------------------------




#|


;; Group elment transformation on vertices, edges, faces. 
(defmethod group-element-apply ((g group-element) (c configuration))
  (make-instance 'configuration
		 :vertices (loop 
			      for v in (configuration.vertices c)
			      collecting (cons (group-element-apply g (car v))
					       (cdr v)))
		 :edges (loop
			   for e in (configuration.edges c)
			   collecting (cons (group-element-apply g (car e))
					    (cdr e)))
		 :faces (loop
			   for f in (configuration.faces c)
			   collecting (cons (group-element-apply g (car f))
					    (cdr f)))))
;; Convenience function
(defun make-face-configuration (face-data)
  (make-instance 'configuration
		 :faces (loop
			   for f in *tetrahedron-faces*
			   and fd in face-data
			   collecting (cons f fd))))

;; Testing
(defparameter ffcc1 (make-face-configuration '(r g b w)))

;; Generate all face colorings (256 of them).
;; Red, green, blue, white.
(defparameter *all-face-configurations*
  (let ((colors '(r g b w))
	(result nil))
    (loop 
       for a in colors 
       do (loop 
	     for b in colors
	     do (loop for c in colors 
		   do (loop for d in colors 
			 do (push (make-face-configuration (list a b c d)) 
				  result)))))
    result))

;; Is ((1 2 3) . b) equal to ((3 2 1) . b) ? 
;; Should be yes.
(defun face-pair-equal-p (fp1 fp2)
  (and (not (set-exclusive-or (car fp1) (car fp2)))
       (eq (cdr fp1) (cdr fp2))))

(defun face-pair-data-equal-p (f1 f2)
  (not (set-exclusive-or f1 f2 :test #'face-pair-equal-p)))

(defun face-configuration-equal-p (fc1 fc2)
  (face-pair-data-equal-p (configuration.faces fc1)
			  (configuration.faces fc2)))

;; Testing.
(defparameter aa1 '(((1 2 3) . b) ((2 3 4) . c) ((1 3 4) . d)))
(defparameter aa2 '(((3 4 1) . d) ((1 3 2) . b) ((4 2 3) . c)))
(defparameter aa3 '(((1 2 4) . b) ((2 3 4) . c) ((1 3 4) . d)))

;; I don't know why we must eliminate symmetrical configurations from
;; both *all-face-configurations* AND *solutions*. Somehow we are not
;; catching some of the ones pushed onto *solutions*.

(defparameter *solution* nil)

(defun tetrahedron-4-colors ()
  (let ((current nil)
	(rotations nil))
    
    (loop 
       until (null *all-face-configurations*)
       do 
	 (setf current (pop *all-face-configurations*))
	 (setf rotations (loop for g in *tetrahedral-group*
			      collecting (group-element-apply g current)))
	 (setf *solution* (set-difference *solution*
					  rotations
					  :test #'face-configuration-equal-p))
	 (setf *all-face-configurations* (set-difference *all-face-configurations*
							 rotations
							 :test #'face-configuration-equal-p))
	 (push current *solution*))
    
    
    (format t "~{~a~%~}" *solution*)
    (length *solution*)))
    
       




;; ==================== for testing ===========================



(defparameter c1 (make-configuration :vertex-data '(r r b b) 
				     :edge-data '(w w w r r r) 
				     :face-data '(r g b w)))


|#

