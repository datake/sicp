
(define (stream-null? s) (null? s))
;(cons-stream a b)=(cons a (delay b))
(define the-empty-stream'())
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (show x)
  (display-line x)
  x)


(define (display-line x)
  (newline)
  (display x))

(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)
