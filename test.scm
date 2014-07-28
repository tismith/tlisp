(load "stdlib.scm")

(write "test cases")

(define contfunc #f)
(write (and
 (cond ((> 1 3) #f) ((> 3 1) #t))
 (case (* 3 2) ((1 2) #f) ((3 4 5 6) #t))
 (case (+ 1 (call-with-current-continuation
  (lambda (cont)
    (set! contfunc cont)
     1))) ((2) #t) (else #f))))
