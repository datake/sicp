;公約数を求めるプログラムであり、以下利用する
(define (gcd a b)
  (if (= b 0) 
      a 
      (gcd b (remainder a b))
   )
)

;わる数が０ならばエラーを吐かせ約分を定義する
;どのような分数を作っても分子に負の符号がつくようにする
(define (make-rat n d)
	(let ((g (gcd n d))
		 ( cond ((= d 0) (error "zero division"))
		 		((< d 0) (cons (/ (- n ) g) (/ (-d) g))
				((> d 0) (cons (/ n g) (/ d g)) 
		  )
	)
)
;分母と分子を定義する
(define (numer x) (car x))
(define (denom x ) (cdr x))



(define (print-rat x)
;もし分母が１なら分子のみを返す
  (newline)
  (if (= (denom x) 1) 
      (display (numer x))
  ((display (numer x))
  (display "/")
  (display (denom x))
x)))

;足し算を定義
(define (add-rat x y)
   (make-rat (+ (* ( numer x) (denom y))
	        (* (numer y) (denom x)))
	     (* (denom x) (denom y))))
; 引き算を定義
(define (sub-rat x y)
   (make-rat (- (* ( numer x) (denom y))
	        (* (numer y) (denom x)))
	     (* (denom x) (denom y))))
;掛け算を定義
(define (mul-rat x y)
   (make-rat (* ( numer x) (numer y))
	        (* (denom y) (denom x))))
;割り算を定義
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
;等しい時#t 等しくない時#f を返す
(define (equal-rat? x y)
  (= (* (numer x)(denom y))
     (* (numer y)(denom x))))
;試しに計算しやすいように定義しておく
(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define one-forth (make-rat 1 4))	    