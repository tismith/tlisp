(load "stdlib.scm")

(write "test cases")

(define call/CC call-with-current-continuation)

(define (search wanted? lst)
  (call/CC
    (lambda (return)
      (for-each (lambda (element)
		  (if (wanted? element)
		    (return element)))
		lst)
      #f)))

(define contfunc #f)
(write (and
 (cond ((> 1 3) #f) ((> 3 1) #t))
 (case (* 3 2) ((1 2) #f) ((3 4 5 6) #t))
 (= (search (lambda (x) (> x 3)) '(1 2 3 4 5)) 4)
 (case (+ 1 (call-with-current-continuation
  (lambda (cont)
    (set! contfunc cont)
     1))) ((2) #t) (else #f))))

