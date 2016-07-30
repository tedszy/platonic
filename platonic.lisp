(in-package #:platonic)

;; Group elements are represented by permutations of 
;; length-(n+1) vectors. The 0th component of the vector
;; us unused for now. This is to make permutations begin
;; at index 1 like in all textbooks.

(defclass group-element ()
  ((name :accessor group-element.name :initarg :name :initform nil)
   (permutation :accessor group-element.permutation :initarg :permutation)))

;; Might want an option for cyclic notiation in the future.
(defmethod print-object ((g group-element) stream)
  (with-slots (name permutation)
      g
    (format stream 
	    "(~a ~{~a~^ ~})"
	    name
	    (loop for i from 1 below (length permutation)
		 collecting (svref permutation i)))))

(defgeneric group-compose (g h)
  (:documentation "compose two group elements by composing permutaions"))

(defgeneric group-equal (g h)
  (:documentation "Are two group elements equal?"))

;; (1 2 3 4)         (1 2 3 4)         (1 2 3 4) 
;; (4 3 2 1) => g    (2 1 4 3) => h    (3 4 1 2) => gh
(defmethod group-compose ((g group-element) (h group-element))
  (loop with perm = (make-array (length (group-element.permutation g))
				:initial-element 0)
     for i from 1 below (length (group-element.permutation g))
     do (setf (svref perm i) (svref (group-element.permutation g) 
				    (svref (group-element.permutation h) i)))
     finally (return (make-instance 'group-element
				    :permutation perm))))

(defmethod group-equal ((g group-element) (h group-element))
  (equalp (group-element.permutation g)
	  (group-element.permutation h)))

(defun g* (&rest args)
  (reduce #'group-compose args))

(defun make-group-element (&rest data)
  ;; if (car data) is not an integer, it is the name of the group
  ;; elment. If it is an integer, then we know name is nil.
  ;; We may use symbols, strings or even lists as names (who knows
  ;; what we will need in the future? So better to say 'not ingegerp'.
  (let ((name (if (not (integerp (car data)))
		  (car data)
		  nil))
	(d (if (integerp (car data))
		 data
		 (cdr data))))
  (make-instance 'group-element
		 :name name
		 :permutation (apply #'vector (cons 0 d)))))

;; Tetrahedral group generators.
;; Standard vertex-edge-face labelling (see graphic).
;; r => 120 degree twist through axis on vertex1 and base 1.
;; s => 180 degree twist through axis on midpoints of edge 14 and 23.
(defparameter r (make-group-element 1 3 4 2))
(defparameter s (make-group-element 4 3 2 1))

;; A4/Tetrahedral group.
(defparameter tetrahedral-group
  (list (make-group-element 'ss   1 2 3 4)
	(make-group-element 'rrsr 3 4 1 2)
	(make-group-element 's    4 3 2 1) 

	(make-group-element 'rsrr 2 1 4 3)
	(make-group-element 'r    1 3 4 2) 
	(make-group-element 'rr   1 4 2 3)

	(make-group-element 'rrs 3 2 4 1)
	(make-group-element 'sr  4 2 1 3)
	(make-group-element 'rs  2 4 3 1)
	
	(make-group-element 'srr 4 1 3 2)
	(make-group-element 'rsr 2 3 1 4)
	(make-group-element 'srs 3 1 2 4)))


;; configuration/state class. the group elements act on this.
;; it could be colorings of faces, vertices or something more
;; creative.


