;1.42
	 
(define (compose f g) 
	(lambda (x) (f (g x))))
 ;例のプログラムも実行してみる。
(define ( inc x)
	(+ x 1))
(define (square x)
	(* x x))
;((compose square inc ) 6)