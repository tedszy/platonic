(in-package #:platonic)

;; Group elements are represented by permutations of 
;; length-(n+1) vectors. The 0th component of the vector
;; us unused for now. This is to make permutations begin
;; at index 1 like in all textbooks.

;; It may be useful to give group elements symbolic names like s or r or id. 
(defclass group-element ()
  ((name :accessor group-element.name :initarg :name :initform nil)
   (permutation :accessor group-element.permutation :initarg :permutation)))

;; Might want option for cyclic notiation in the future.
(defmethod print-object ((g group-element) stream)
  (with-slots (name permutation)
      g
    (format stream 
	    "<~a ~a>"
	    name
	    permutation)))

(defgeneric group-operation (g h))

(defmethod group-operation ((g group-element) (h group-element))
  
  )

(defun make-group-element (&rest data)
  (make-instance 'group-element
		 :permutation (apply #'vector data)))


;; configuration/state class. the group elements act on this.
;; it could be colorings of faces, vertices or something more
;; creative.


