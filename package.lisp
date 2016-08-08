(defpackage #:platonic
  (:use #:cl)
  (:export #:run-tests
	   #:*tetrahedron-group*
	   #:*cube-group*
	   #:make-group-element
	   #:g*
	   #:has-identity-property-p
	   #:has-inverse-property-p
	   #:has-closure-property-p
	   #:transform-vef
	   #:transform-configuration
	   #:faces
	   #:make-tetrahedron-configuration
	   #:distinct-tetrahedron-colorings
	   #:make-cube-configuration
	   #:distinct-cube-colorings
	   #:burnside-tetrahedron
	   #:burnside-cube
  ))
