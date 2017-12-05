;formal parameter
;actual argument

;applicative order: ﬁrst evaluates the operator and operands and then apply
;normal order: fully expand and then reduce

;(define (p) (p) )
;ConditionalExpressionsandPredicates
(define (square x) (* x x))
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

;tree recursion
(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
	(cond 
		((= amount 0) 1) 
		((or (< amount 0) (= kinds-of-coins 0)) 0) 
		(else 
			(+ 
				(cc amount (- kinds-of-coins 1)) 
				(cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
			)
		)
	)
)

(define (first-denomination kinds-of-coins) 
	(cond 
		((= kinds-of-coins 1) 1) 
		((= kinds-of-coins 2) 5) 
		((= kinds-of-coins 3) 10) 
		((= kinds-of-coins 4) 25) 
		((= kinds-of-coins 5) 50)
	)
) 

;ex1.11
;f(n) = n ;if n < 3, 
;f(n) = f(n−1)+2f(n−2)+3f(n−3) ;if n ≥ 3. 
(define (fex111recur n)
	(cond 
		((< n 3) n )
		(else 
			(+
				(fex111recur (- n 1))
				(* 2 (fex111recur (- n 2)))
				(* 3 (fex111recur (- n 3))) 
			
			)
		)
	)

)

(define (fex111iter n)
	(define (fex111iter1 a b c count)
		(cond
			((= count 0) c)
			(else 
				(fex111iter1 b c (+ c (* 2 b)(* 3 a)) (- count 1) )
			)
		)	
	)
	(cond
		((< n 3) n )
		(else
			(fex111iter1 0 1 2 (- n 2))
		)
	)
)

;pi/4 = 1-1/3+1/5-1/7+...
;higher-order procedures
;(display (random 100))
;(display (milliseconds))

(define (sum-recur term a next b)
	(if (> a b) 0
	(+ (term a)
		(sum-recur term (next a) next b) 
	))
)

(define (succ n) (+ n 1))
(define (prec n) (- n 1))
(define (identity x) x)
(define (const a b) a )

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* dx
	(sum f (+ a (/ dx 2.0)) add-dx b))
)

;ex1.30
(define (sum-iter term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result  a))
		)
	)
	(iter a a)
)

;ex1.32.b
(define (accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result a))
		)
	)
	(iter a a)
)

;lambda
(define plus4 
	(lambda (x) (+ x 4))
)

;let
(define (cubexy x y)
	(let
	(
		(a (* x x x))
		(b (* 3 x x y))
		(c (* 3 x y y))
		(d (* y y y))
	)
	(+ a b c d))
)


(define (linera-combination a b x y )
	(add (mul a x) (mul b y) )
)

(define (addRational x y)
	(makeRational 	(+ (* (numer x) (denom y) ) (* (numer y) (denom x) ) )
	 				(* (denom x) (denom y) ) 
	)
)

(define (subRational x y)
	(makeRational 	(- (* (numer x) (denom y) ) (* (numer y) (denom x) ) )
	 				(* (denom x) (denom y) ) 
	)
)
;Chapter 2

(define makeRational cons)
(define numer car)
(define denom cdr)
(define (mulRational x y)
	(makeRational (* (numer x)(numer y))
				  (* (denom x)(denom y))
	)
)

(define (divRational x y)
	(makeRational (* (numer x)(denom y))
				  (* (denom x)(numer y))
	)
)

(define (equalRational x y)
	(= 	(*(numer x)(denom y))
		(*(denom x)(numer y))
	)
)

;pairs
(define x (cons 1 2))

(define (printRational x)
	(newline)
	(dispaly (numer x)) 
	(display "/")
	(display (denom x))
)



