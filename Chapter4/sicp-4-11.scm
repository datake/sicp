
;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
     (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;main of ex.4.11
;;;((var1 var2 var3 ..)(val1 val2 val3..))->((var1 val1)(var2 val2)(var3 val3))と環境演算を書き直す


;;; (make-frame (var1 var2 var3)(val1 val2 val3))->
;;; (cons (var1 val1) iter( (var2 var3)(val2 val3)))
;;; (cons (var1 val1) (cons((var2 val2) iter(var3)(val3))))
;;; (cons (var1 val1) (cons((var2 val2) (cons(var3)(val3)))))

(define (make-frame variables values)
  (define (iter variables values)
    (if (null? variables)
	'()
	(cons (cons (car variables) (car values))
	      (iter (cdr variables) (cdr values)))))

  (iter variables values))



;;;((var1 val1)(var2 val2)(var3 val3))
;;;->(cons (var1) frame-variables((var2 val2)(var3 val3)))
;;;->(cons (var1 var2)  frame-variables(var3 val3))
;;;->(cons (var1 var2 var3) frame-variables(null) ) 
;;;->(var1 var2 var3) 


(define (frame-variables frame)
	(if (null? frame)
		'()
		(cons (caar frame)
			(frame-variables (cdr frame)))))


;;;((var1 val1)(var2 val2)(var3 val3))
;;;->(cons (val1) frame-variables((var2 val2)(var3 val3)))
;;;->(cons (val1 val2)  frame-variables(var3 val3))
;;;->(cons (val1 val2 val3) frame-variables(null) ) 
;;;->(val1 val2 val3) 

(define (frame-values frame)
	(if (null? frame)
		'()
		(cons (cdar frame)
			(frame-valueues (cdr frame)))))



;; (add-binding-to-frame! var0 val0 ((var1 val1)(var2 val2)(var3 val3)))
;; (add-first (list(cons( var0 val0))) ((var2 val2)(var3 val3)))
;; (add-first (list(cons( var0 val0)))(var3 val3))

;; (add-first (list(cons( var0 val0)))(val3))
;; (set-cdr!  (val3) (list(cons( var0 val0))) )


;; (add-last (list(cons( var0 val0)))(var3 val3))


;; ((var0 val0) (var1 val1)(var2 val2)(var3 val3))


(define (add-binding-to-frame! variable value frame)
	(define (add-last bind f)
		(if (null? (cdr f))
			(set-cdr! f bind)
			(add-last bind (cdr f))))
	(add-last (list (cons variable value)) frame))

;実行例
(define frame-test (make-frame '(a b) '(0 1)))
(define env-test (list frame))
