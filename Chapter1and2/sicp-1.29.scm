(define (integral-simpson f a b n)) 
;nは偶整数
(define h (/ (- b a) n))
;(y k)をy_{k}に3/hをかけたものとする
(define (y k) ((* (/ h 3) f (+ a (* k h)))))

;addは変数を１たす操作
(define (add x)(+ x 1))
;sumを抽象化
(define (sum  term a )
        (cond((= a 0)
      	    (+ (y 0) (sum (add a) ))))
	(cond ((= a n)
	       (+ (y a))))
	(cond((odd? a)
	       (+ (* 4 (y a))(sum (add a) ))))
 	(cond((even? a)
	       (+ (* 2 (y a))(sum (add a) )))))

	       



;      (+ (term p)
;	 (sum term (next p) next q))))
;y0とynの係数は1で、y_{奇数}の時係数は4y_{偶数}の時係数は2である
