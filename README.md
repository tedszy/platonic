# Platonic
Using groups to compute distinct ways of coloring regular solids. Platonic can find all distinct colorings for tetrahedrons and cubes. 

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
NIL
CL-USER> (ql:quickload :platonic)
To load "platonic":
  Load 1 ASDF system:
    platonic
; Loading "platonic"
```

Now you can run the tests and see some nice output.

```
CL-USER> (platonic:run-tests)
```

### Tetrahedron


### Cube

