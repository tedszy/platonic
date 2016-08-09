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

```
CL-USER> (platonic:run-tests)
```

If you don't want to keep typing the package-name prefix ```platonic:``` you can go inside the packages and work there. We will do it that way here. If you choose to work outside the package, remember that you have to resolve all symbols defined in Platonic, and that includes symbols like ```platonic:faces```. Look in ```package.lisp``` to see the exported symbols.

```
CL-USER> (in-package :platonic)
#<PACKAGE "PLATONIC">
PLATONIC> 
```

The SBCL prompt shows that we are in the platonic package.


## Groups




## Tetrahedron
Rotational symmetries are represented by permutation vectors that act on vertices, edges or faces. The rotation group for the tetrahedron contains 12 elements. Let's take two and compose them.

```
PLATONIC> (setq a (nth 4 *tetrahedron-group*))
#(0 3 2 4 1)
CL-USER> (setq b (nth 7 platonic:*tetrahedron-group*))
#(0 2 3 1 4)
CL-USER> (platonic:g* a b)
#(0 2 4 3 1)
```

Now create a face coloring configuration and transform it with group elements.

```
CL-USER> (setq tc 
	           (platonic:make-tetrahedron-configuration 'platonic:faces 
			                                            '(r g b y)))
(PLATONIC:FACES ((1 2 3) R) ((1 2 4) G) ((1 3 4) B) ((2 3 4) Y))
```

Transform it.

```
CL-USER> (platonic:transform-configuration (platonic:g* a b) tc)
(PLATONIC:FACES ((1 2 3) B) ((1 2 4) G) ((1 3 4) Y) ((2 3 4) R))
```

And now compute all distinct colorings with three colors.

```
CL-USER> (platonic:distinct-tetrahedron-colorings '(r g b))
((PLATONIC:FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) G) ((2 3 4) G))
 (PLATONIC:FACES ((1 2 3) R) ((1 2 4) G) ((1 3 4) G) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) G) ((2 3 4) R))
 (PLATONIC:FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) G) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) B) ((1 2 4) B) ((1 3 4) R) ((2 3 4) R))
 (PLATONIC:FACES ((1 2 3) R) ((1 2 4) G) ((1 3 4) B) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) B) ((1 2 4) G) ((1 3 4) R) ((2 3 4) R))
 (PLATONIC:FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) B) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) G) ((1 2 4) G) ((1 3 4) R) ((2 3 4) R))
 (PLATONIC:FACES ((1 2 3) R) ((1 2 4) B) ((1 3 4) B) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) B) ((1 2 4) R) ((1 3 4) R) ((2 3 4) R))
 (PLATONIC:FACES ((1 2 3) G) ((1 2 4) B) ((1 3 4) B) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) G) ((1 2 4) R) ((1 3 4) R) ((2 3 4) R))
 (PLATONIC:FACES ((1 2 3) B) ((1 2 4) B) ((1 3 4) B) ((2 3 4) B))
 (PLATONIC:FACES ((1 2 3) R) ((1 2 4) R) ((1 3 4) R) ((2 3 4) R)))
```

There are 15 of them!


## Cube
It's much the same for the cube, except that the rotational symmetry group is more complcated, having 24 elements:

```
CL-USER> platonic:*cube-group*
(#(0 1 2 3 4 5 6 7 8) #(0 2 3 4 1 6 7 8 5) #(0 3 4 1 2 7 8 5 6)
 #(0 4 1 2 3 8 5 6 7) #(0 4 3 7 8 1 2 6 5) #(0 8 7 6 5 4 3 2 1)
 #(0 5 6 2 1 8 7 3 4) #(0 5 1 4 8 6 2 3 7) #(0 6 5 8 7 2 1 4 3)
 #(0 2 6 7 3 1 5 8 4) #(0 1 4 8 5 2 3 7 6) #(0 1 5 6 2 4 8 7 3)
 #(0 6 2 1 5 7 3 4 8) #(0 3 2 6 7 4 1 5 8) #(0 6 7 3 2 5 8 4 1)
 #(0 8 4 3 7 5 1 2 6) #(0 3 7 8 4 2 6 5 1) #(0 8 5 1 4 7 6 2 3)
 #(0 2 1 5 6 3 4 8 7) #(0 7 3 2 6 8 4 1 5) #(0 7 8 4 3 6 5 1 2)
 #(0 5 8 7 6 1 4 3 2) #(0 4 8 5 1 3 7 6 2) #(0 7 6 5 8 3 2 1 4)) 
```

How many distinct ways are there to color a cube with three colors?

```
CL-USER> (length (platonic:distinct-cube-colorings '(r g b)))
57
```

57 ways! 

