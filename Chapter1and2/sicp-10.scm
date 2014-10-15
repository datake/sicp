;beside���`����
(define (beside painter1 painter 2)
	(let ((split-point (make-vect 0.5 0.0)))
		(let ((paint-left
		(transform-painter
			painter1
			(make-vect 0.0 0.0)
			split-point 
			(make-vect 
			
			
			
			
;�ȉ����W�����ɕύX�����B
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
;�ȏヌ�W�����ɕύX
			
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



  	
			












;identity�̏�ƉE���ꂼ��Q���������A�ċA�I�ɉE���identity������
;corner-split���`����
(define (coner-split painter n)
;n�͕�������񐔂�\���ĂO�̎����ꎩ�g��\��
	(if (= n 0)
		painter
;identity���c���ɕ�������up�ƁA�����ɕ�������right������
		
		(let ((up (up-split painter (- n 1)))
				(right (right-split painter (- n 1))))
;up�����ɓ���ׂĉ��ɕ\������top-left�Aright���c�ɕ��ׂ�bottom-right,
;��������񐔂�n-1��ɂ����������E��ɕ\������corner������
			(let ((top-left (beside up up))
					(bottom-right (below right right))
					(corner (corner-split painter (- n 1))))
;identity�̏��top-left,�E��bottom-right,�E���corner��z�u����B
				(beside (below painter top-left )
						(below bottom-right corner))))))
						

						
				
;�������̐��������Ȃ���
;identity�̉E�ɏc�ɓ������\������right-split���`����
;n�͕�������񐔂�\���ĂO�̎����ꎩ�g��\��
(define (right-split painter n)
;0�񕡐�����Ƃ���identity��\������
	(if (= n 0)
		painter
;n-1�񕡐�������̂�smaller�Ƃ���
		(let ((smaller (right-split painter ( - n 1))))
;identity�̉E�ɓ��smaller��\������
			(beside painter (below smaller smaller)))))


;identity�̏�ɉ��ɓ������\������up-split���`����
;n�͕�������񐔂�\���ĂO�̎����ꎩ�g��\��
(define up-split  painter n)
	(if (= n 0)
		painter
;0�񕡐�����Ƃ���identity��\������
;n-1�񕡐�������̂�smaller�Ƃ���
		(let ((smaller (up-split (- n 1))))
;identity�̏�ɓ��smaller��\������
		(below painter (beside smaller smaller)))))
		
		
		
		


;�l�����ɕ������쐬����square-limit���`����
(define (square-limit painter n)
;��L�Œ�`����square-limit��quarter�Ƃ����B
	(let ((quarter 
			(corner-split painter n)) )
;quarter�Ƃ���𔽓]���������̂����ɕ��ׂ�half�Ƃ���
		(let ((half (beside
					(flip-horiz quarter)
						quarter)))
;half�Ƃ���𔽓]���������̂��c�ɕ��ׂ�
			(below (flip-vert half)
					half)))) 