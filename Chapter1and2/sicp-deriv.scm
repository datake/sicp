

;xがシンボルであるときtrueを返す(
(define (variable? x) (symbol? x))
;v1とｖ２はそれを表現している記号が同じかどうかを判断する
(define (same-variable? v1 v2)
	(and (variable? v1 ) (variable? v2) (eq? v1 v2)))

;和をリストとして構成する
;このとき0を足す作業は省略しa1とa2が同じ型ならたした結果を表示する

(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
	      ((=number? a2 0) a1)
	      ((and (number? a1) (number? a2))(+ a1 a2))
	      (else (list '+ a1 a2)))) 

;上述の=number?は式が与えられた数に等しいかをみる
(define (=number? exp num)
	(and (number? exp) (= exp num)))


;積をリストとして構成する
;１を掛けるときは１を掛ける操作を省略しｍ１とｍ２が同じ型なら予めかける
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
	　　　((=number? m1 1) m2)
	      ((=number? m2 1) m1)
	      ((and (number? m1) (number? m2)) (* m1 m2))
	      (else (list '* m1 m2))))

;商をリストとして構成する
;０を割るときには０をかえす
;１で割るときには割られる値を返す
(define (make-division d1 d2)
	(cond 
	　　　((=number? d1 0) 0)
	      ((=number? d2 1) d1)
	      ((and (number? d1) (number? d2)) (/ d1 d2))
		(else (list '/ d1 d2)) )) 



;pairはcarやcdrをとれるペアがくるとtrueを返す
;和は最初の要素が＋であるリストである
(define (sum? x)
	(and (pair? x)(eq? (car x) '+)))

;項が２項であれば以下のコメントアウトした部分で十分である
;加数は和のリストの第２項である
(define (addend s)(cadr s))
;被加数は和のリストの第３項である
;(define (augend s) (caddr s))
;任意の項の和を扱えるようにすると以下のようになる
(define (augend s)
	(if (null? (cdddr s))
	(caddr s)
	(cons '+ (cddr s))))


;割る数と割られる数を定義する
(define (divident d) (cadr d))
(define (divisor d) (caddr d))

;商を定義
(define (division? x)
	(and (pair? x) (eq? (car x) '/)) )

;積は最初の要素が*であるリストである
(define (product? x)
	(and (pair? x)(eq? (car x) '*)))

;乗数は積のリストの第２項である
(define (multiplier p)(cadr p))

;項が２項であれば以下のコメントアウトした部分で十分である
;被乗数は積のリストの第３項である
;(define (multiplicand p) (caddr p))
;任意の項の和を扱えるようにすると以下のようになる
(define (multiplicand p)
  (let ((rest (cddr p)))
	(if (null? (cddr p))
	    (car rest)
	    (cons '* rest)))) 


;以下冪乗の操作を定義する
(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e))))

(define base cadr)

(define exponent caddr)

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (deriv exp var)
;定数であったら定数の微分として０を返す
(cond ((number? exp) 0)
	((variable? exp)
;expとvarが同じ変数であれば一次の微分を行う
	　(if (same-variable? exp var) 1 0))

;expが和の形になっていたら線形的に加数と被加数に分解する
	((sum? exp)
	　(make-sum (deriv (addend exp) var)
		(deriv (augend exp) var)))
;expが積の形になっていたら積の微分定理を使う
	((product? exp)
	　(make-sum 
		(make-product (multiplier exp)
			(deriv (multiplicand exp) var))
		(make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
;差を適用させる
;	(- x y) (+ x (* -1 y))
;商に対する微分ルールを追加する
((division? exp)
  (make-sum
	(make-product
	     (make-division
	       (make-product
		(make-product -1
		   (divident exp) )
		(deriv (divisor exp) var) )
	     (make-product
		(divisor exp)
		(divisor exp) )))

	(make-product
	      (make-division 1 (divisor exp))
	      (deriv (divident exp) var) )))

 
;冪乗にたいするルールを追加する
((exponentiation? exp)
;  (make-product
;	(makeproduct 
;	    (exponent exp)
;	    (deriv (base exp) var) )
;	(make-exponentiation
;	    (base exp)
;	    (make-sum (exponent exp) -1) )))

 (make-product (exponent exp)
                       (make-exponentiation (base exp) (make-sum (exponent exp) -1))))

;それ以外であればエラーを吐き出す
(else
	(error "unknown expression typex -- DERIV" exp))))





;参考HP>http://www.serendip.ws/archives/893
;>http://d.hatena.ne.jp/ohyajapan/20090118/p1






