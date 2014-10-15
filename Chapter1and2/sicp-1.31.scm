
(define (product term a next b)
  (if (> a b)
      1
      (*(term a )(product term (next a) next))))

(define (factorial n)
  (deine (next i)(+ i 1))
  (define (term i) i)
  (product term 1 next n))

;nはかける回数
(define (pi n)
  (define (term k)
    (if (even? k)
	(/ (+ k 2)(+ k 1))
	(/ (+ k 1)(+ k 2))))
  (define (next n)
    (+1 n))
(* 4.0 (product term 1 next n)))

;上のproductは再帰的である。
;よって反復的プロセスを示す

(define (product-iter term a next b)
(if (> a b)
    1
    (*(term a)
      (product term (next a) next b))))