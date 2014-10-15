;besideを定義する
(define (beside painter1 painter 2)
	(let ((split-point (make-vect 0.5 0.0)))
		(let ((paint-left
		(transform-painter
			painter1
			(make-vect 0.0 0.0)
			split-point 
			(make-vect 
			
			
			
			
;以下レジュメに変更した。
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                         (let ((t1->t2 (get-coercion type1 type2))
                               (t2->t1 (get-coercion type2 type1)))
                              (cond (t1->t2
                                      (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                      (apply-generic op a1 (t2->t1 a2)))
                                    (else (error "No method for these types" (list op type-tags))))))
                    (error "No method for these types" (list op type-tags)))))))





(define (attach-tag type-tag contents)
	(if (eq? type-tag 'scheme-number)
		contents
		(cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
;     (error "Bad tagged datum -- TYPE-TAG" datum)))
	'scheme-number))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))
;以上レジュメに変更
			
(define (install-scheme-number-package)
	(define (tag x)
		(attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
	     (lambda (x y) (tag (+ x y))))
  	put 'sub '(scheme-number scheme-number)
  	    (lambda (x y) (tag (- x y))))
 	(put 'mul '(scheme-number scheme-number)
      (lambda (x y) (tag (* x y))))
 	(put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
 	(put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
  
 (define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))

  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
 
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
  
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magunitude z1) (magunitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magunitude z1) (magunitude z2))
		       (- (angle z1) (angle z2))))

(define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
  
  (define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



  	
			












;identityの上と右それぞれ２つずつ複製し、再帰的に右上にidentityをつくる
;corner-splitを定義する
(define (coner-split painter n)
;nは複製する回数を表して０の時それ自身を表す
	(if (= n 0)
		painter
;identityを縦長に複製するupと、横長に複製するrightをおく
		
		(let ((up (up-split painter (- n 1)))
				(right (right-split painter (- n 1))))
;upを横に二つ並べて横に表示するtop-left、rightを縦に並べるbottom-right,
;複製する回数をn-1回にした複製を右上に表示するcornerをおく
			(let ((top-left (beside up up))
					(bottom-right (below right right))
					(corner (corner-split painter (- n 1))))
;identityの上にtop-left,右にbottom-right,右上にcornerを配置する。
				(beside (below painter top-left )
						(below bottom-right corner))))))
						

						
				
;かっこの数かぞえなおせ
;identityの右に縦に二つ複製を表示するright-splitを定義する
;nは複製する回数を表して０の時それ自身を表す
(define (right-split painter n)
;0回複製するときはidentityを表示する
	(if (= n 0)
		painter
;n-1回複製するものをsmallerとおく
		(let ((smaller (right-split painter ( - n 1))))
;identityの右に二つsmallerを表示する
			(beside painter (below smaller smaller)))))


;identityの上に横に二つ複製を表示するup-splitを定義する
;nは複製する回数を表して０の時それ自身を表す
(define up-split  painter n)
	(if (= n 0)
		painter
;0回複製するときはidentityを表示する
;n-1回複製するものをsmallerとおく
		(let ((smaller (up-split (- n 1))))
;identityの上に二つsmallerを表示する
		(below painter (beside smaller smaller)))))
		
		
		
		


;四方向に複製を作成するsquare-limitを定義する
(define (square-limit painter n)
;上記で定義したsquare-limitをquarterとおく。
	(let ((quarter 
			(corner-split painter n)) )
;quarterとそれを反転させたものを横に並べてhalfとする
		(let ((half (beside
					(flip-horiz quarter)
						quarter)))
;halfとそれを反転させたものを縦に並べる
			(below (flip-vert half)
					half)))) 