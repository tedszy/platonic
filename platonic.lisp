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

(defgeneric group-element-equalp (g h)
  (:documentation "T if two group elements are equal."))

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

(defmethod group-element-equalp (g h)
  (equalp (group-element.permutation g)
	  (group-element.permutation h)))

;; Ordering within faces and edges and so on should not matter.
;; Vertex has one label, edge has two, faces have three or more.
;; If all are represented by lists of integers (including verticies,
;; which are lists of one integer) then we have a uniform interface.

(defclass geometric () ((label-list :accessor geometric.label-list :initarg :label-list)))

(defun make-geometric (label-list)
  (make-instance 'geometric :label-list label-list))

(defmethod print-object ((geo geometric) stream) 
  (format stream "[~{~a~^ ~}]" (geometric.label-list geo))) 
			
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

;; Data lists.
(defun make-configuration (&key (vertex-data nil) (edge-data nil) (face-data nil))
  (let ((vertices (mapcar #'make-geometric '((1) (2) (3) (4))))
	(edges (mapcar #'make-geometric '((1 2) (1 3) (1 4) (2 3) (3 4) (2 4))))
	(faces (mapcar #'make-geometric '((1 2 3) (1 3 4) (1 4 2) (2 3 4)))))
    (make-instance 'configuration
		   :vertices (loop 
				for v in vertices
				and vd in vertex-data
				collecting (cons v vd))
		   :edges (loop 
			     for e in edges
			     and ed in edge-data
			     collecting (cons e ed))
		   :faces (loop 
			     for f in faces
			     and fd in face-data
			     collecting (cons f fd)))))

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




;; ==================== for testing ===========================

(defparameter c1 (make-configuration :vertex-data '(r r b b) 
				     :edge-data '(w w w r r r) 
				     :face-data '(r g b w)))

(defparameter fc1 (make-face-config '(r g b w)))  


;; Tetrahedral rotational symmetry group generators.
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
