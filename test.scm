(load "stdlib.scm")

(and
 (cond ((> 1 3) #f) ((> 3 1) #t))
 (case (* 3 2) ((1 2) #f) ((3 4 5 6) #t)))
