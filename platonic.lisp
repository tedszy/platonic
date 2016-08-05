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

(defclass geometric () ((label-list :accessor geometric.label-list :initarg :label-list)))

(defun make-geometric (label-list)
  (make-instance 'geometric :label-list (sort label-list #'<)))

(defmethod print-object ((geo geometric) stream) 
  (format stream "[~{~a~^ ~}]" (geometric.label-list geo))) 

;; Compare two vertices/edges/faces.
(defun geometric-equal-p (g h)
  (not (set-exclusive-or (geometric.label-list g)
			 (geometric.label-list h))))

;; Testing
(defparameter gg1 (make-geometric '(3 2 1)))
(defparameter gg2 (make-geometric '(2 1 3)))
			
(defgeneric group-element-apply (g v)
  (:documentation "Transform a vertex/edge/face by applying group element."))

;; Transform vertex/edge/face.
(defmethod group-element-apply ((g group-element) (geo geometric))
  (make-geometric (loop for v in (geometric.label-list geo)
		     collecting (svref (group-element.permutation g) v))))

;; Slots consist of alists.
(defclass configuration ()
  ((vertices :accessor configuration.vertices :initarg :vertices :initform nil)
   (edges :accessor configuration.edges :initarg :edges :initform nil)
   (faces :accessor configuration.faces :initarg :faces :initform nil)))

(defmethod print-object ((c configuration) stream)
  (format stream
	  "~&(vertices ~{~a~^ ~}~& edges ~{~a~^ ~}~& faces ~{~a~^ ~}"
	  (configuration.vertices c)
	  (configuration.edges c)
	  (configuration.faces c)))

(defparameter *tetrahedron-vertices* 
  (mapcar #'make-geometric '((1) (2) (3) (4))))
(defparameter *tetrahedron-edges* 
  (mapcar #'make-geometric '((1 2) (1 3) (1 4) (2 3) (3 4) (2 4))))
(defparameter *tetrahedron-faces* 
  (mapcar #'make-geometric '((1 2 3) (1 3 4) (1 4 2) (2 3 4))))

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

;; Acts on vertices, edges, faces. 
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

;; Specialize to just faces.
(defclass face-config (configuration) ())

(defmethod print-object ((fc face-config) stream)
  (format stream
	  "~&(faces ~{~a~^ ~}"
	  (configuration.faces fc)))
    
(defun make-face-config (face-data)
  (let ((faces (mapcar #'make-geometric '((1 2 3) (1 3 4) (1 4 2) (2 3 4)))))
    (make-instance 'face-config
		   :faces (loop 
			     for f in faces
			     and fd in face-data
			     collecting (cons f fd)))))

(defmethod group-element-apply ((g group-element) (fc face-config))
  (make-instance 'face-config
		 :faces (loop
			   for f in (configuration.faces fc)
			   collecting (cons (group-element-apply g (car f))
					    (cdr f)))))

;; Define group of tetrahedral rotational symmetries. Two generators r, s.

;; Standard vertex-edge-face labelling (see graphic).
;; r => 120 degree twist through axis on vertex1 and base 1.
;; s => 180 degree twist through axis on midpoints of edge 14 and 23.
(defparameter s  (make-group-element 4 3 2 1)) 
(defparameter r  (make-group-element 1 3 4 2)) 

(defparameter tetrahedral-group
  (list s r 
	(g* s s) (g* r r) (g* s r) (g* r s)
	(g* r s r) (g* r r s) (g* s r r)
	(g* r r s r) (g* r s r r)))

(defparameter tgroup
  (mapcar #'(lambda (u) 
	      (apply #'make-group-element u)) 
	  '((1 2 3 4) (3 4 1 2) (4 3 2 1)
	    (2 1 4 3) (1 3 4 2) (1 4 3 2)
	    (3 2 4 1) (4 2 1 3) (2 4 3 1)
	    (4 1 3 2) (2 3 1 4) (3 1 2 4))))

;; Generate all face colorings (256 of them).
;; Red, green, blue, white.
(defparameter all-face-configs
  (mapcar #'make-face-config
	  (let ((colors '(r g b w))
		(result nil))
	    (loop 
	       for a in colors 
	       do (loop 
		     for b in colors
		     do (loop for c in colors 
			   do (loop for d in colors 
				 do (push (list a b c d) result)))))
	    result)))

;; When are two face configurations equivalent?
;; A simple comparison of alists. If each pair in fc1's alist has 
;; equivalent in fc2 then they are equal.
(defun equivalent-face-configs-p (fc1 fc2)
  (let ((a1 (mapcar #'(lambda (f) 
			(cons (geometric.label-list (car f))
			      (cdr f)))
		    (configuration.faces fc1)))
	(a2 (mapcar #'(lambda (f) 
			(cons (geometric.label-list (car f))
			      (cdr f)))
		    (configuration.faces fc2))))
    (null (set-difference a1 a2 :test #'equalp))))



;; pop top of config list and put in solution list.
;; delete-if all equivalent configs under symmetry group.
;; pop next one
;; repeat until config list is empty.


(defparameter solution nil)

(defun solve ()
  (loop 
     until (null all-face-configs)
     do 
       (push (pop all-face-configs) solution)
       (loop 
	  for g in tgroup
	  do (setf all-face-configs 
		   (delete-if #'(lambda (u)
				  (equivalent-face-configs-p (group-element-apply g (car solution))
							     u))
			      all-face-configs)))))



;; ==================== for testing ===========================

;; something seems to be wrong with equivalence and filtering.

 (defparameter to-remove 
   (loop for g in tgroup
      collecting (group-element-apply g (car all-face-configs))))

;; should have length 244
(defparameter foo
  (set-difference all-face-configs to-remove :test #'equivalent-face-configs-p))

;; (delete-if #'(lambda (u) (equivalent-face-configs-p fc2 u))
;;		     all-face-configs)



(defparameter c1 (make-configuration :vertex-data '(r r b b) 
				     :edge-data '(w w w r r r) 
				     :face-data '(r g b w)))

(defparameter fc1 (make-face-config '(r g b w)))  
(defparameter fc2 (group-element-apply r fc1))


#|

;; Ordering for faces. (1 2 5) < (1 3 4). We cannot have
;; two equal faces in the same solid.
(defun face< (fa fb)
  (loop 
     with ra = (reverse (sort fa #'<))
     with rb = (reverse (sort fb #'<))
     with multiplier = 1
     with resa = 0
     with resb = 0
     for a in ra
     and b in rb
     do 
       (incf resa (* a multiplier))
       (incf resb (* b multiplier))
       (setf multiplier (* multiplier 10))
     finally (return (< resa resb))))
 
(defun face= (fa fb)
  (equalp (sort fa #'<)
	  (sort fb #'<)))


|#
