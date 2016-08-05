(in-package #:platonic)

;; Group elements are represented by permutations of 
;; length-(n+1) vectors. The 0th component of the vector
;; us unused for now. This is to make permutations begin
;; at index 1 like in all textbooks.

(defclass group-element ()
  ((permutation :accessor group-element.permutation :initarg :permutation)))

(defmethod print-object ((g group-element) stream)
  (let ((p (group-element.permutation g)))
    (format stream 
	    "<~{~a~^ ~}>" 
	    (loop for i from 1 below (length p)
		 collecting (svref p i)))))

(defun make-group-element (&rest data)
  (make-instance 'group-element
		 :permutation (apply #'vector (cons 0 data))))

(defgeneric group-element-* (g h)
  (:documentation "compose two group elements by composing the permutations 
that define them."))

;; (1 2 3 4)         (1 2 3 4)         (1 2 3 4) 
;; (4 3 2 1) => g    (2 1 4 3) => h    (3 4 1 2) => g*h
(defmethod group-element-* ((g group-element) (h group-element))
  (let ((pg (group-element.permutation g))
	(ph (group-element.permutation h)))
    (loop with perm = (make-array (length pg) :initial-element 0)
	 for i from 1 below (length pg)
	 do (setf (svref perm i) (svref pg (svref ph i)))
	 finally (return (make-instance 'group-element
					:permutation perm)))))

;; Convenience.
(defmethod g* (&rest elements) (reduce #'group-element-* elements))

;; Ordering within faces and edges and so on should not matter.
;; Vertex has one label, edge has two, faces have three or more.
;; If all are represented by lists of integers (including verticies,
;; which are lists of one integer) then we have a uniform interface.

;; ****************

;; Actually, making this a class rather than just lists
;; complicates my life quite a bit.

;; Compare two vertices/edges/faces.
(defun geometric-equal-p (g h) (not (set-exclusive-or g h)))

;; Testing
(defparameter gg1 '(3 2 1))
(defparameter gg2 '(2 1 3))
			
(defgeneric group-element-apply (g v)
  (:documentation "Transform a vertex/edge/face by applying group element."))

;; Transform vertex/edge/face.
(defmethod group-element-apply ((g group-element) (geo list))
  (loop for v in geo
     collecting (svref (group-element.permutation g) v)))


;; ============ Tetrahedron setup ====================

(defparameter *tetrahedron-vertices* '((1) (2) (3) (4)))
(defparameter *tetrahedron-edges* '((1 2) (1 3) (1 4) (2 3) (3 4) (2 4)))
(defparameter *tetrahedron-faces* '((1 2 3) (1 3 4) (1 4 2) (2 3 4)))

;; Define group of tetrahedral rotational symmetries. Two generators r, s.

;; Standard vertex-edge-face labelling (see graphic).
;; r => 120 degree twist through axis on vertex1 and base 1.
;; s => 180 degree twist through axis on midpoints of edge 14 and 23.
(defparameter s  (make-group-element 4 3 2 1)) 
(defparameter r  (make-group-element 1 3 4 2)) 

(defparameter *tetrahedral-group*
  (list s r 
	(g* s s) (g* r r) (g* s r) (g* r s)
	(g* r s r) (g* r r s) (g* s r r)
	(g* r r s r) (g* r s r r)))

(defparameter tgroup
  (mapcar #'(lambda (u) 
	      (apply #'make-group-element u)) 
	  '(;;(1 2 3 4) 
	    ;; (3 4 1 2) (4 3 2 1)
	    ;; (2 1 4 3) (1 3 4 2) (1 4 3 2)
	    ;; (3 2 4 1) (4 2 1 3) (2 4 3 1)
	    ;; (4 1 3 2) (2 3 1 4) 
	    (3 1 2 4)
	    )))

;; ==================================================

;; Slots consist of alists.
(defclass configuration ()
  ((vertices :accessor configuration.vertices :initarg :vertices :initform nil)
   (edges :accessor configuration.edges :initarg :edges :initform nil)
   (faces :accessor configuration.faces :initarg :faces :initform nil)))

;; Checks if slot is nil, ignores it if so.
(defmethod print-object ((c configuration) stream)
  (with-slots (vertices edges faces)
      c
    (when (not (null vertices))
      (format stream "~&(vertices ~{~a~^ ~}" vertices))
    (when (not (null edges))
      (format stream "~&(edges ~{~a~^ ~}" edges))
    (when (not (null faces))
      (format stream "~&(faces ~{~a~^ ~}" faces))
    (when (and (null vertices) (null edges) (null faces))
      (format stream "~&(configuration nil)"))))

;; Data lists.
(defun make-configuration (&key (vertex-data nil) (edge-data nil) (face-data nil))
    (make-instance 'configuration
		   :vertices (loop 
				for v in *tetrahedron-vertices*
				and vd in vertex-data
				collecting (cons v vd))
		   :edges (loop 
			     for e in *tetrahedron-edges*
			     and ed in edge-data
			     collecting (cons e ed))
		   :faces (loop 
			     for f in *tetrahedron-faces*
			     and fd in face-data
			     collecting (cons f fd))))
;; Testing.
(defparameter cc1 (make-configuration :vertex-data '(r g b w)
				      :edge-data '(r r r r r r)
				      :face-data '(b b g g)))


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




