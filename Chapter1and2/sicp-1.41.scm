;1.41
;( define (double function x)
;	( function (function x)))
;(define ( inc x)
;	(+ x 1))

;手続きとあるのでlambdaをつかってしめすと以下のようになる
;http://www.serendip.ws/archives/476を参考にした
(define (double function)
	(lambda (x) (function (function x)
		    )
	)
)
(define (inc)
	(lambda (x) (+ x 1)))
