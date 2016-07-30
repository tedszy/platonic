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
	    "<~a ~a>"
	    name
	    (loop for i from 1 below (length permutation)
		 collecting (svref permutation i)))))

(defgeneric group-compose (g h)
  (:documentation "compose two group elements by composing permutaions"))

(defgeneric group-equal (g h)
  (:documentation "Are two group elements equal?"))

;; (1 2 3 4)
;; (4 3 2 1) => g
;;
;; (1 2 3 4)
;; (2 1 4 3) => h
;;
;; (1 2 3 4) 
;; (3 4 1 2) => gh
(defmethod group-compose ((g group-element) (h group-element))
  (loop
     with perm = (make-array (length (group-element.permutation g))) 
     for u across (group-element.permutation g)
     and v across (group-element.permutation h)
     and i from 1 below (length (group-element.permutation g))
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
  (make-instance 'group-element
		 :permutation (apply #'vector (cons 0 data))))

(defparameter gg (make-group-element 4 3 2 1))
(defparameter hh (make-group-element 2 1 4 3))

;; Tetrahedral group generators.
(defparameter rr (make-group-element 1 3 4 2))
(defparameter ss (make-group-element 4 1 2 3))

;; configuration/state class. the group elements act on this.
;; it could be colorings of faces, vertices or something more
;; creative.


