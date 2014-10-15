(define (make-account balance password);passwordを追加
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                     balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
    
    ;ここでpasswordに関する記述を追加
  (define (dispatch password_dispatch m)
    (if (eq? password_dispatch password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (error "Incorrect Password")))
  dispatch)
  
  (define acc (makke-account 100 'secret-password))