(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
       (define (set-front-ptr! item) (set! front-ptr item))
       (define (set-rear-ptr! item) (set! rear-ptr item))
       (define (empty-queue?) (null? front-ptr))
       (define (front-queue)
         (if (empty-queue?)
             (error "FRONT called with an empty queue")
             (car front-ptr)))
       (define (rear-queue)
         (if (empty-queue?)
             (error "REAR called with on empty queue")
             (car rear-ptr)))
       (define (insert-queue! item)
         (let ((new-pair (cons item '())))
              (cond ((empty-queue?)
                     (set-front-ptr! new-pair)
                     (set-rear-ptr! new-pair)
                     front-ptr)
                    (else
                      (set-cdr! rear-ptr new-pair)
                      (set-rear-ptr! new-pair)
                      front-ptr))))
       (define (delete-queue!)
         (cond ((empty-queue?)
                (error "DELETE! called with an empty queue"))
               (else
                 (set-front-ptr! (cdr front-ptr)))))
     
       (define (dispatch m)
         (cond ((eq? m 'insert-queue!) insert-queue!)
               ((eq? m 'delete-queue!) delete-queue!)
               ((eq? m 'front-queue) front-queue)
               ((eq? m 'rear-queue) rear-queue)
               (else
                 (error "Unknown request --MAKE-QUEUE" m))))

       dispatch))


(define q1 (make-queue))


((q1 'insert-queue!) 'a)


((q1 'insert-queue!) 'b)


((q1 'insert-queue!) 'c)

(q1 'delete-queue! )
(q1 'delete-queue! )
(q1 'delete-queue! )
(q1 'delete-queue! )
(q1 'unknown-request-queue )


#|実行例
(define q1 (make-queue))
=> #lambda
((q1 (quote insert-queue!)) (quote a))
=> (a)
((q1 (quote insert-queue!)) (quote b))
=> (a b)
((q1 (quote insert-queue!)) (quote c))
=> (a b c)
(q1 (quote delete-queue!))
=> #lambda
(q1 (quote delete-queue!))
=> #lambda
(q1 (quote delete-queue!))
=> #lambda
(q1 (quote delete-queue!))
=> #lambda
(q1 (quote unknown-request-queue))
=> #error<Unknown request --MAKE-QUEUE>
|#
