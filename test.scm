(load "stdlib.scm")

(define call/cc call-with-current-continuation)

(define (search wanted? lst)
  (call/cc
    (lambda (return)
      (for-each
	(lambda (element)
	  (if (wanted? element)
	    (return element)))
	lst)
      #f)))

(define (f return)
    (return 2)
      3)

(define contfunc #f)

(and
  (cond ((> 1 3) #f) ((> 3 1) #t))
  (case (* 3 2) ((1 2) #f) ((3 4 5 6) #t))
  (= (search (lambda (x) (> x 3)) '(1 2 3 4 5)) 4)
  (case (+ 1 (call-with-current-continuation
	       (lambda (cont)
		 (set! contfunc cont)
		 1))) ((2) #t) (else #f))
  (= 3 (f (lambda (x) x)))
  (= (call-with-current-continuation f) 2)
)


