;sicp 3.1

(define (make-accumulator sum)
  (lambda (n)
          (begin (set! sum (+ sum n))
                 sum)))

(define A (make-accumulator 5))

(A 10)
(A 10)



;(define (make-accumulator sum) (lambda (n) (begin (set! sum (+ sum n)) sum)))
;=> #lambda
;(define a (make-accumulator 5))
;=> #lambda
;(a 10)
;=> 15
;(a 10)
;=> 25
; (lambda (シンボル1 シンボル2 ... シンボルm) 式1 式2 ... 式n)

;letの使い方
;(let ((変数１ 初期値１)
;     (変数２ 初期値２)
;        ・・・・・・
;      (変数Ｍ 初期値Ｍ))

;    Ｓ式１
;  ・・・・・・
;    Ｓ式Ｍ)
