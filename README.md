# Platonic
Using group theory and brute-force to compute distinct ways of coloring regular solids, Platonic can find all ways to distinctly color tetrahedrons and cubes. The examples below involve coloring faces, but coloring of edges and vertices are also supported. In principle you can attach any sort of information to vertices, edges and faces and ask for the number of distinct configurations. 

## Setup
If you don't have Steel Bank Common Lisp (SBCL), Qucklisp, Slime
and Emacs set up, [watch this video.](https://www.youtube.com/watch?v=VnWVu8VVDbI) Once you have that set up (as well as git), clone this repo into quicklisp/local-projects.

```
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/tedszy/platonic.git
```

You will have to let Quicklisp know that the project is there. Start Emacs, do ```M-x slime```, register the project and load it.

```
CL-USER> (ql:register-local-projects)
CL-USER> (ql:quickload :platonic)
```

Now you can run the tests and see some nice output.

```common-lisp
CL-USER> (platonic:run-tests)
```

If you don't want to keep typing the package-name prefix ```platonic:``` you can go inside the packages and work there. We will do it that way here. If you choose to work outside the package, remember that you have to resolve all symbols defined in Platonic, and that includes symbols like ```platonic:faces```. Look in ```package.lisp``` to see the exported symbols.

```common-lisp
CL-USER> (in-package :platonic)
#<PACKAGE "PLATONIC">
PLATONIC> 
```

The SBCL prompt shows that we are in the platonic package.


## Groups
Group elements are represented by length n+1 permutation vectors. These vectors begin at index 1. The 0th index is unused. This is to conform with the convention used in all textbooks: a typical permutation is ```(1 3 2 4)``` not ```(0 2 1 3)```. We can create group elements and compose them with group multiplication ```g*```. For example,

```common-lisp
PLATONIC> (setq g1 (make-group-element '(1 4 2 3)))
#(0 1 4 2 3)
PLATONIC> (setq g2 (make-group-element '(4 2 3 1)))
#(0 4 2 3 1)
PLATONIC> (setq g3 (make-group-element '(2 3 1 4)))
#(0 2 3 1 4)
PLATONIC> (g* g1 g2 g3)
#(0 4 2 3 1)
```

Geometrical entites like verticies, edges and faces are handled with a uniform interface: they are all simply lists. A vertex is a list of one label, an edge has two, and a face has three or more. Group element can be appled to vertices, edges or faces (vefs), e.g.,

```common-lisp
PLATONIC> (transform-vef g1 '(1 3 4))
(1 2 3)
```

How do we know if a list of permutation vectors actually forms a group? We can test the group properties: unique inverses, unique identity and closed multiplication table. Let's remove the 7th element from the tetrahedron group and watch it fail the group tests.

```common-lisp
PLATONIC> (setq am-i-a-group (loop for g in *tetrahedron-group*
                                   for i from 1
				                   unless (= i 7) collect g))
(#(0 1 2 3 4) #(0 1 3 4 2) #(0 1 4 2 3) #(0 4 2 1 3) #(0 3 2 4 1) #(0 4 1 3 2)
 #(0 2 3 1 4) #(0 3 1 2 4) #(0 4 3 2 1) #(0 2 1 4 3) #(0 3 4 1 2))
PLATONIC> (has-identity-property-p am-i-a-group)
T
PLATONIC> (has-closure-property-p am-i-a-group)
NIL
PLATONIC> (has-inverse-property-p am-i-a-group)
NIL
```

It fails two of the tests.


## Tetrahedron
The rotation group for the tetrahedron contains 12 elements. A face configuration is a list of faces and their colors. Create a face configuration and transform it by applying group elements:

```common-lisp
PLATONIC> (setq tc (make-tetrahedron-configuration 'faces '(r g b y)))
(FACES ((1 2 3) R) ((1 2 4) G) ((1 3 4) B) ((2 3 4) Y))
PLATONIC> (transform-configuration g1 tc)
(FACES ((1 2 3) B) ((1 2 4) R) ((1 3 4) G) ((2 3 4) Y))
PLATONIC> (transform-configuration (g* g1 g2 g3) tc)
(FACES ((1 2 3) Y) ((1 2 4) G) ((1 3 4) B) ((2 3 4) R))
```

Compute all distinct colorings with three colors.

```common-lisp
PLATONIC> (distinct-tetrahedron-colorings '(r g b))
((FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) G) ((2 3 4) G))
 (FACES ((1 2 3) R) ((1 2 4) G) ((1 3 4) G) ((2 3 4) B))
 (FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) G) ((2 3 4) R))
 (FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) G) ((2 3 4) B))
 (FACES ((1 2 3) B) ((1 2 4) B) ((1 3 4) R) ((2 3 4) R))
 (FACES ((1 2 3) R) ((1 2 4) G) ((1 3 4) B) ((2 3 4) B))
 (FACES ((1 2 3) B) ((1 2 4) G) ((1 3 4) R) ((2 3 4) R))
 (FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) B) ((2 3 4) B))
 (FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) R) ((2 3 4) R))
 (FACES ((1 2 3) R) ((1 2 4) B) ((1 3 4) B) ((2 3 4) B))
 (FACES ((1 2 3) B) ((1 2 4) R) ((1 3 4) R) ((2 3 4) R))
 (FACES ((1 2 3) G) ((1 2 4) B) ((1 3 4) B) ((2 3 4) B))
 (FACES ((1 2 3) G) ((1 2 4) R) ((1 3 4) R) ((2 3 4) R))
 (FACES ((1 2 3) B) ((1 2 4) B) ((1 3 4) B) ((2 3 4) B))
 (FACES ((1 2 3) R) ((1 2 4) R) ((1 3 4) R) ((2 3 4) R)))
```

There are 15 of them!

## Cube
It's much the same for the cube, except that the rotational symmetry group is more complcated, having 24 elements:

```common-lisp
PLATONIC> *cube-group*
(#(0 1 2 3 4 5 6 7 8) #(0 2 3 4 1 6 7 8 5) #(0 3 4 1 2 7 8 5 6)
 #(0 4 1 2 3 8 5 6 7) #(0 4 3 7 8 1 2 6 5) #(0 8 7 6 5 4 3 2 1)
 #(0 5 6 2 1 8 7 3 4) #(0 5 1 4 8 6 2 3 7) #(0 6 5 8 7 2 1 4 3)
 #(0 2 6 7 3 1 5 8 4) #(0 1 4 8 5 2 3 7 6) #(0 1 5 6 2 4 8 7 3)
 #(0 6 2 1 5 7 3 4 8) #(0 3 2 6 7 4 1 5 8) #(0 6 7 3 2 5 8 4 1)
 #(0 8 4 3 7 5 1 2 6) #(0 3 7 8 4 2 6 5 1) #(0 8 5 1 4 7 6 2 3)
 #(0 2 1 5 6 3 4 8 7) #(0 7 3 2 6 8 4 1 5) #(0 7 8 4 3 6 5 1 2)
 #(0 5 8 7 6 1 4 3 2) #(0 4 8 5 1 3 7 6 2) #(0 7 6 5 8 3 2 1 4))
PLATONIC> 
```

How many distinct ways are there to color a cube with three colors?

```common-lisp
PLATONIC> (length (distinct-cube-colorings '(r g b)))
57
```

57 ways! 

