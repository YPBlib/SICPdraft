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

(define (average x y)
	(/ (+ x y) 2)
)

;square root by Newton's root
(define (improve guess x)
	(average guess (/ x guess))
)

(define (goodEnough guess x)
	(< (abs (- (square guess) x)) 0.001)
)

(define (sqrtIter guess x)
	(if (goodEnough guess x)
		guess
		(sqrtIter(improve guess x) x)
	) 
)

(define (linearFactorial n)
	(define (linearFactorialIter pro current)
		(if (> current n)
			pro
			(linearFactorialIter 
				(* pro current )
				(+ current 1)
			)
		)
	)
	(linearFactorialIter 1 1 )
)

;recursive procedure: syntactic fact that the procedure definition refers to the procedure itself
;recursive process: how the process evolves


