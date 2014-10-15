
(define (hilbert-a p0 q0 p1 q1 i)
	(let ((xs (/ (+ (* 3.0 p0) p1) 4.0))
		(ys (/ (+ (* 3.0 q0) q1) 4.0))
		(xm (/ (+ p0 p1) 2.0))
		(ym (/ (+ q0 q1) 2.0))
		(xl (/ (+ p0 (* 3.0 p1)) 4.0))
		(yl (/ (+ q0 (* 3.0 q1)) 4.0)) )

		(if (= i 0)
			(list (make-vect pl ql) (make-vect xs ql)
				(make-vect xs ys) (make-vect xl ys))
			(append (hilbert-d xm ym p1 q1 (- i 1))
				(hilbert-a p0 ym xm q1 (- i 1))
				(hilbert-a p0 q0 xm ym (- i 1))
				(hilbert-b xm q0 p1 ym (- i 1))
			) 
		)
	)
)


(define (hilbert-b p0 q0 p1 q1 i)
	(let ((xs (/ (+ (* 3.0 p0) p1) 4.0))
		(ys (/ (+ (* 3.0 q0) q1) 4.0))
		(xm (/ (+ p0 p1) 2.0))
		(ym (/ (+ q0 q1) 2.0))
		(xl (/ (+ p0 (* 3.0 p1)) 4.0))
		(yl (/ (+ q0 (* 3.0 q1)) 4.0)) )

		(if (= i 0)
			(list (make-vect xs ys) (make-vect xs q1)
				(make-vect p1 q1) (make-vect p1 ys))
			(append (hilbert-c p0 q0 xm ym (- i 1))
				(hilbert-b p0 ym xm q1 (- i 1))
				(hilbert-b xm ym p1 q1(- i 1))
				(hilbert-a xm q0 p1 ym (- i 1))) )))



(define (hilbert-c p0 q0 p1 q1 i)
	(let ((xs (/ (+ (* 3.0 p0) p1) 4.0))
		(ys (/ (+ (* 3.0 q0) q1) 4.0))
		(xm (/ (+ p0 p1) 2.0))
		(ym (/ (+ q0 q1) 2.0))
		(xl (/ (+ p0 (* 3.0 p1)) 4.0))
		(yl (/ (+ q0 (* 3.0 q1)) 4.0)) )

		(if (= i 0)
			(list (make-vect xs ys) (make-vect p1 ys)
				(make-vect p1 q1) (make-vect xs q1))
			(append (hilbert-b p0 q0 xm ym (- i 1))
				(hilbert-c xm q0 p1 ym (- i 1))
				(hilbert-c xm ym p1 q1 (- i 1))
				(hilbert-d p0 ym xm q1 (- i 1))) )))

(define (hilbert-d p0 q0 p1 q1 i)
	(let ((xs (/ (+ (* 3.0 p0) p1) 4.0))
		(ys (/ (+ (* 3.0 q0) q1) 4.0))
		(xm (/ (+ p0 p1) 2.0))
		(ym (/ (+ q0 q1) 2.0))
		(x1 (/ (+ p0 (* 3.0 p1)) 4.0))
		(y1 (/ (+ q0 (* 3.0 q1)) 4.0)) )

		(if (= i 0)
			(list (make-vect xl yl) (make-vect xl ys)
				(make-vect xs ys) (make-vect xs yl))
				
				
			(append (hilbert-a xm ym p1 q1 (- i 1))
				(hilbert-d xm ym p1 ym (- i 1))
				(hilbert-d p0 q0 xm ym (- i 1))
				(hilbert-c p0 ym xm q1 (- i 1))) )))

(define vectors->segments (lambda (vectors)
	(define vector->segment (lambda (vector-list)
		(define a (car vector-list))
		(define b (cadr vector-list))
		(if (eq? nil b)
			'()
			(append (list (make-segment 
				(make-vect (car a) (cdr a))
				(make-vect (car b) (cdr b))))
				(vector->segment (cdr vector-list))) )))
	(vector->segment vectors) ))







(define (hilbert n)
	(segments->painter
		(vectors->segments (hilbert-a 0.0 0.0 1.0 1.0 n))) )
