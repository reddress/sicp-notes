;;; Structure and Interpretation of Computer Programs
;;; Using Unofficial Texinfo format 2.andresraba3

;;; p. 33 Giving names to identify variables and assign them values

(define pi 3.14159)
(define radius 10)

(* pi (* radius radius))
;; 314.159

;;; p. 37 Procedure definitions

(define (square x)
  (* x x))

(square (+ 2 5))
;; 49

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

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

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
