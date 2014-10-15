(define (make-monitored f )
	(let (count 0))
	(define (mf x)
		(define (how-many-calls x )
			count)
		(define (reset x)
			(begin (set! count 0)
				0))
		
		(define (add_count x)
			(begin (set! count 
				(+ 1 count))))
	)			
				
	(define (dispatch m)
		(cond ((eq? m 'how-many-calls?) how-many-calls)
			((eq? m 'reset ) reset)
			(else add_count))))
)

(define (sqrt x)
	(* x x)
)