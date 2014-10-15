(define (circle z)
	(let ((r (+ (expt (- (car z) 0.5) 2)
				(expt (- (cdr z) 0.5) 2) )))
		(if  (< r 0.006)
			#xff0000
			#xffffff) ))

((procedure->painter circle) frm1)