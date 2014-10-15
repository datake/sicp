;/fiをaとおく
;a^_{2}=a+1であり変換x→１+1/xをfとする
;不動点であることを示すにはf(a)=aであることをしめせばよい
;左辺=f(a)=1+1/a=1+2/(1+√5)=1+(1-√5)/2=a=右辺
;よって示せた

;fixed-point関数をつくる教科書どおりのプログラム
(define tolerance 0.00001)
(define (abs x)
  (cond ((> x 0) x)
	((< x 0) - x)
	((x = 0) 0)))
(define (fixed-point f first-guess)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))
(define(try guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
    next
    (try next))))
(try first-guess))
