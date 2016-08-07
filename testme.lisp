(in-package #:platonic)
		
;; Burnside's formula for tetrahedron.
;; c = number of colors.	
(defun burnside (c)
  (/ (+ (* 11 (* c c))
	(* c c c c))
     12))

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