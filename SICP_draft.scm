;formal parameter
;actual argument

;applicative order: ﬁrst evaluates the operator and operands and then apply
;normal order: fully expand and then reduce

(define (p) (p) )
;ConditionalExpressionsandPredicates
(define (abs1 x)
	(cond 
		((> x 0) x)
		((< x 0 )(- x))
		((= x 0) 0) 
	)
)
(define (abs2 x)
	(cond 
		((< x 0) (- x))
		(else x)
	)
)

(define (abs3 x)
	(if (< x 0) (- x)
		x
	)
)

(define (predicatex1 x)
	(and (< x 10) (> x 0) )
)

(define (predicatex2 x)
	(or (> x 10) (< x 0) )
)

(define (predicatex3 x)
	(not (= x 0))
)

(define (>= x y)
	(or (= x y) (> x y) )
)

(define (<= x y)
	(not (> x y) )
)



(define
	(test x y )
	(if (= x 0) 0 y)
)

(define (sqrt x)

	(define (good-enough? guess x)
		(< (abs (- (square guess) x)) 0.001)
	)

	(define (improve guess x) 
		(average guess (/ x guess))
	)

	(define (sqrt-iter guess x)
		(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x))
	)
	(sqrt-iter 1.0 x)
)
