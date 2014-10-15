;recursion
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (-n1))
		 (fib (- n 2)) ))))
;iteration
(define (fib-i n)
  (define (iter a b count)
    (if (= count 0)
	b
	(iter (+ a b) a (- count 1)) ))
  (iter 1 0 n) )