;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BUILDING ABSTRACTIONS WITH PROCEDURES

;;; p. 33 Giving names to identify variables and assign them values

(define pi 3.14159)
(define radius 10)

;; (* pi (* radius radius))
;;;; 314.159

;;; p. 37 Procedure definitions

(define (square x)
  (* x x))

;; (square (+ 2 5))
;;;; 49

;;; p. 43 Case analysis with conditionals

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;;; or

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;;; or

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;;; p. 57 Internal definitions

(define (sqrt x)
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; p. 60 Linear recursive process

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;; p. 61 Linear iterative process

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;; p. 65 Tree recursion

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

;;; p. 67 Iterative process for computing Fibonacci numbers

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;;; p. 69 Ways to count change in coins (a tree-recursive process)

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;; p. 75 Fast exponentiation

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;;; p. 79 GCD with Euclid's Algorithm

;;; GCD(a, b) = GCD(b, r), r is the remainder when a is divided by b

(define (gcd a b)
  (if (= b 0)
       a
       (gcd b (remainder a b))))

;;; if b > a then r = a, resulting in a and b switching places

;;; p. 81 Testing primality by searching for divisors

(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (prime? n)
  (= n (smallest-divisor n)))

;;; p. 82 Exponential of a number modulo another number

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square
           (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base
             (expmod base (- exp 1) m))
          m))))

;;; p. 83 Fermat's test for primality (a probabilistic method)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;;; p. 90 Summation template

;; (define (<name> a b)
;;   (if (> a b)
;;       0
;;       (+ (<term> a)
;;          (<name> (<next> a) b))))

;;; p. 91 Concept of summation

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

;;; p. 92 Numerical integration

(define (integral f a b)
  (let ((dx 0.2))   ;; stack overflow if dx = 0.001 or large a-b range
    (define (add-dx x)
      (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

;;; p. 95 Constructing procedures using lambda

;;; A way of directly specifying "the procedure that returns its input incremented by 4

;; (lambda (x) (+ x 4))

;; is the same procedure as
(define plus4 (lambda (x) (+ x 4)))
  
(define (integral-with-lambda f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;;; p. 98 Let

;;; The general form of a let expression is
;; (let ((var-1 exp-1)
;;       (var-2 exp-2)
;;       ...
;;       (var-n exp-n))
;;   body)

;;; let is equivalent to a lambda

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;; is the same as
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;;; p. 101 Finding roots of equations

(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (define (average x y) (/ (+ x y) 2))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Given values are not of opposite sign" a b)))))

;;; p. 103 Fixed points of functions

(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2))
         tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

;; does not converge
;; (define (sqrt-fixed-point x)
;;   (fixed-point (lambda (y) (/ x y)) 1.0))

;;; p. 104 Average damping

(define (average x y) (/ (+ x y) 2))
(define (sqrt-average-damping x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;; p. 107 Procedures as returned values

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;; p. 109 Numerical derivative

(define (deriv g)
  (let ((dx 0.001))
    (lambda (x)
      (/ (- (g (+ x dx))
            (g x))
         dx))))

;;; p. 109 Newton's method

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newtons-method x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

;;; p. 110 Fixed point of transform

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

