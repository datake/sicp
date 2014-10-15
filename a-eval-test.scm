(define f (analyze '(cons 1 2)))

(f the-global-environment)

(define g (analyze '(cons z 2)))

(g the-global-environment)

(g (extend-environment '(z) '(2) the-global-environment))

(g (extend-environment '(z) '(#t) the-global-environment))


