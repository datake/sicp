

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                     balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
    
  
  (define (dispatch password_dispatch m)
    (if (eq? password_dispatch password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (error "Incorrect Password")))
  dispatch)
 (define acc (makke-account 100 'secret-password)) 
  
  
(define (make-joint account  old-password new-password)
	(define (dispatch password m)
		(if(eq? password m)
			(account old-password m)		
			(error "Incorrect Password")))
	dispatch)
	


(define peter-acc (make-account 100 'open-sesame))

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
  


(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 20)
 
  
 
 (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'deposit) 10)
((paul-acc 'wrongrosebud 'deposit) 10)

