
(define mypainter1
  (
;(clear-picture)
   (set-color #xf1bb93)
	      (vertexes->painter
	       (list
		(make-vect .20 .30) (make-vect .80 .30)
		(make-vect .80  .30) (make-vect .80 .60)
		(make-vect .80 .60) (make-vect .20 .60)
		(make-vect .20 .60) (make-vect .20 .30) )
	       #t)
))
  