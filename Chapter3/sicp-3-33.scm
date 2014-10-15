(define true #t)
(define false #f)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;プローブは指示されたコネクタの設定あや設定解除についてのメッセージを刻印
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)
;コネクタが値をもつかどうか
(define (has-value? connector)
  (connector 'has-value?))
;コネクタの現在の値を返す
(define (get-value connector)
  (connector 'value))
;通知者はコネクタに新しい値を設定するよう要求していることを示す
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
;撤回者がその値を忘れるように要求していることをコネクタに告げる
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
;コネクタに新しい制約に関わるよう告げる
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; コネクタの表現
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
  (define (me request)
    (cond ((eq? request 'has-value?)
           (if informant true false))
          ((eq? request 'value) value)
          ((eq? request 'set-value!) set-my-value)
          ((eq? request 'forget) forget-my-value)
          ((eq? request 'connect) connect)
          (else (error "Unknown operation --CONNECTOR"
                       request))))
    me))




; 加算器
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

; 乗算器
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)


; 反復子
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))


; constant構成子
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;平均をとる手続き
(define (averager a b c )
  (let ((connector1 (make-connector))
    (connector2 (make-connector)))
    (adder a b connector1)
    (multiplier c connector2 connector1)
    (constant 2 connector2)
    'ok))

;テスト


(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(define p (make-connector))
(define q (make-connector))
(define r (make-connector))

(averager a b c)
;c=(a+b)/2
(averager p q r)
;r=(p+q)/2
(probe "a" a)
(probe "b" b)
(probe "c" c)
(set-value! a 100 'user)
(set-value! b 200 'user)

(probe "p" p)
(probe "q" q)
(probe "r" r)
(set-value! q 100 'user)
(set-value! r 200 'user)
