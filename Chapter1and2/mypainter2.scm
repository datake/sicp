(define mypainter2
   ((set-color #x000000)
     (segments->painter
	       (list
		(make-vect .45 .55) (make-vect .40 .50)
		(make-vect .40  .50) (make-vect .40 .40)
		(make-vect .40 .40) (make-vect .45 .35)
		(make-vect .45 .35) (make-vect .55 .35)
		(make-vect .55 .35) (make-vect .60 .40)
		(make-vect .60 .40) (make-vect .60 .50)
		(make-vect .60 .50) (make-vect .55 .55)
		(make-vect .55 .55) (make-vect .45 .55))
	       )
))