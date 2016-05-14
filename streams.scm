;;; Using Chicken
;;; p. 396 Building streams

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

;;; avoid endless loop
(define (stream-for-ten proc s)
  (define (iter proc s count)
    (if (> count 10)
        (begin (newline) 'first-ten)
        (begin (proc (stream-car s))
               (iter proc (stream-cdr s) (+ count 1)))))
  (iter proc s 0))

(define (display-stream-first-ten s)
  (stream-for-ten display-line s))

;;; must be special form
;;; (define (cons-stream a b) (cons a (delay-sicp b)))

;;; http://stackoverflow.com/questions/13998388/running-code-from-sicp-section-3-5-4-with-drracket
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

;;; p. 398 Stream car and cdr

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define (delay-sicp exp)
  (memo-proc (lambda () exp)))

(define (force-sicp delayed-object) (delayed-object))

(define (test-stream-implementation)
  (stream-car
   (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000
                                              1100000)))))

;;; p. 404 Infinite streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (test-stream-ref) 
  (stream-ref no-sevens 100))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;;; p. 406 Sieve

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(define (find-prime)
  (stream-ref primes 50))

;;; p. 407 Defining streams implicitly

(define ones (cons-stream 1 ones))


;;; p. 403 Exercise 3.50 Generalized stream-map

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;;; p. 414

;; from code.scm
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-stream x)
  (define (sqrt-improve guess x)
    (average guess (/ x guess)))
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;;; Ex. 3.55

;;; See finite version in experimental.scm

;; 1 2 3  4  5
;;   1 2  3  4  
;;     1  2  3  
;;        1  2  
;;           1  
;; ------------
;; 1 3 6 10 15

(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

(define (check-partial-sums)
  (display-stream-first-ten (partial-sums (stream-enumerate-interval 1 18))))

;;; p. 416 Euler sequence

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (use-acceleration)
  (display-stream-first-ten
   (accelerated-sequence euler-transform pi-stream)))

;;; p. 419 Infinite streams of pairs

;; (define (find-pair-sum-prime)
;;   (stream-filter
;;    (lambda (pair) (prime? (+ (car pair) (cadr pair))))
;;    int-pairs))

;; (define (rest-of-first-row)
;;   (stream-map (lambda (x) (list (stream-car s) x))
;;               (stream-cdr t)))

;; (define (pairs s t)
;;   (cons-stream
;;    (list (stream-car s) (stream-car t))
;;    (<combine-in-some-way>
;;     (stream-map (lambda (x) (list (stream-car s) x))
;;                 (stream-cdr t))
;;     (pairs (stream-cdr s) (stream-cdr t)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;; p. 425 Streams as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;;; p. 430 Will not work because the call to integral requires dy to be defined, which only happens in the next line
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
                    int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (test-solver)
  (stream-ref (solve (lambda (y) y) 1 0.001) 1000))

;;; p. 436 Stream of random numbers

(define rand
  (let ((random-init 17))
    (let ((x random-init))
      (lambda ()
        (set! x (rand-update x))
        x))))

(define (rand-update x)
  (let ((a 27)  ;; obtained from official code in ch3support.scm
        (b 26)
        (m 127))
    (remainder (+ (* a x) b) m)))


(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define random-numbers
  (let ((random-init 117))
    (cons-stream random-init
                 (stream-map rand-update random-numbers))))

;;; p. 436 Monte Carlo with streams

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))
(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))

;;; p. 439 Streams as a way of storing time-dependent data

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))

;;; Next: see evaluator.scm

