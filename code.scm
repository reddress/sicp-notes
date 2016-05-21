;;; Structure and Interpretation of Computer Programs
;;; Using Unofficial Texinfo format 2.andresraba3

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
  (let ((dx 0.01))   ;; stack overflow if dx = 0.001 or large a-b range
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BUILDING ABSTRACTIONS WITH DATA

;;; p. 117 Data abstraction

;; (define (linear-combination a b x y)
;;   (add (mul a x) (mul b y)))

;;; p. 121 Rational numbers

;;; make-rat, numer, and denom defined below
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; p. 122 Pairs
;;; defined with cons, whose parts can be extracted with car and cdr

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;;; p. 130 Data structure as procedures

(define (cons-proc x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument must be 0 or 1: cons-proc" m))))
  dispatch)  ;; the return value is this procedure

(define (car-proc z) (z 0))
(define (cdr-proc z) (z 1))

;;; p. 139 Representing a sequence of pairs as a list

;; (list <a-1> <a-2> ... <a-n>)

;; is equivalent to
;; (cons <a-1>
;;       (cons <a-2>
;;             (cons ...
;;                   (cons <a-n>
;;                         '() ... ))))  ;; '() is given as nil

;;; p. 141 List operations

(define (list-ref-sicp items n)
  (if (= n 0)
      (car items)
      (list-ref-sicp (cdr items) (- n 1))))

(define (length-sicp items)
  (if (null? items)
      0
      (+ 1 (length-sicp (cdr items)))))

(define (append-sicp list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-sicp (cdr list1) list2))))

;;; p. 146 Mapping over lists

(define (map-sicp proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map-sicp proc (cdr items)))))

;;; p. 151 Recursive procedures on trees

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;;; p. 154 Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;;; p. 158 Filtering a sequence is selecting only those elements that satisfy a given predicate

(define (filter-sicp predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter-sicp predicate (cdr sequence))))
        (else (filter-sicp predicate (cdr sequence)))))

;;; p. 158 Accumulation

(define (accumulate-sicp op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate-sicp op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;; p. 155 Sequences as conventional interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

;; is equivalent to the signal-flow of (p. 159)

(define (sum-odd-squares-signal-flow tree)
  (accumulate-sicp
   + 0 (map-sicp square (filter-sicp odd? (enumerate-tree tree)))))

;;; another example (p. 155)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; is equivalent to the signal-flow of (p. 159)

(define (even-fibs-signal-flow n)
  (accumulate-sicp
   cons
   '()
   (filter-sicp even? (map-sicp fib (enumerate-interval 0 n)))))

;;; p. 160 Data processing, assume we have a 'salary' selector for a programmer

;; (define (salary-of-highest-paid-programmer records)
;;   (accumulate
;;    max 0 (map salary (filter programmer? records))))

;;; p. 166 Nested mappings

(define (nested-mapping-example)
  (let ((n 3))
    (accumulate-sicp
     append '() (map-sicp (lambda (i)
                            (map-sicp (lambda (j) (list i j))
                                      (enumerate-interval 1 (- i 1))))
                          (enumerate-interval 1 n)))))

(define (flatmap proc seq)
  (accumulate-sicp append '() (map-sicp proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter-sicp prime-sum?
                    (flatmap
                     (lambda (i)
                       (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
                     (enumerate-interval 1 n)))))

;;; p.167 Permutations (depends on flatmap and remove)

(define (remove item sequence)
  (filter (lambda (x) (not (equal? x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;;; p. 189 Quotation distinguishes between symbols and their values

;; (define a 1)
;; (define b 2)
;; (list a b)
;;;; (1 2)
;; (list 'a 'b)
;;;; (a b)
;; (list 'a b)
;;;; (a 2)

;; '() is the empty list, nil is no longer used from this point forward

;;; p. 190 Check if a symbol is contained in a list

(define (memq-sicp item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq-sicp item (cdr x)))))

;;; p. 223 Multiple representations for abstract data: complex numbers

;;; Assume four selectors:
;;; real-part, imag-part, magnitude, and angle

;;; constructors 
;;; (make-from-real-imag (real-part z) (imag-part z))
;;; (make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;; p. 224
;;; Ben Bitdiddle chooses rectangular form
;;; Alyssa P. Hacker chooses polar form

;;; p. 226 Tagged data

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged data: type-tag" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged data: contents" datum)))

(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

;;; p. 227 Rectangular form
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;;; p. 227 Polar form
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang r a)
  (attach-tag 'polar
              (cons r a)))

;;; p. 228 Generic selectors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: real-part" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: imag-part" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: magnitude" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: angle" z))))

;;; p. 232 Data-directed programming: manipulating the operation-and-type table

;; (put <op> <type> <item>)  ;; installs the <item> in the table, indexed by <op> and <type>

;; (get <op> <type>)  ;; looks up the <op> and <type> entry and returns the item found there, or False if it is not found

;;; p. 336 put and get are implemented in Section 3.3.3, Two-dimensional tables, with the names lookup and insert!

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'OK)

(define complex-table (make-table))

(define (put op type item)
  (insert! op type item complex-table))

(define (get op type)
  (lookup op type complex-table))

;;; p. 233 Installing a representation package

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  ;;; type is contained in a list because apply-generic uses map on the list of args
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;; p. 235 A general "operation" procedure

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: apply-generic"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; p. 239 Message passing

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: make-from-real-imag" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;;; p. 250 Coercion

;; (define (scheme-number->complex n)
;;   (make-complex-from-real-imag (contents n) 0))

;;; install coercion procedures in a special coercion table
;; (put-coercion 'scheme-number
;;               'complex
;;               scheme-number->complex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODULARITY, OBJECTS, AND STATE

;;; p. 279 Local state variables

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;;; p. 281 Make state variable internal to procedure

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;;; p. 282 Withdrawal processors

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

;;; p. 283 Withdraw and deposit

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: make-account" m))))
  dispatch)

;;; p. 286 Random number sequence

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
    (modulo (+ (* a x) b) m)))

;;; p. 287 Cesaro test with Monte Carlo method for estimating pi

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;;; p. 323 Sharing an individual pair

(define x (list 'a 'b))
(define z (cons x x))

;;; p. 327 Implement mutable data objects as procedures (compare to representation on p. 130)

(define (cons-mutable x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else
           (error "Undefined operation: cons-mutable" m))))
  dispatch)

(define (car-mutable z) (z 'car))
(define (cdr-mutable z) (z 'cdr))
(define (set-car-mutable! z new-value)
  ((z 'set-car!) new-value) z)
(define (set-cdr-mutable! z new-value)
  ((z 'set-cdr!) new-value) z)

;;; p. 330 Representing queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "front-queue called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "delete-queue! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;;; p. 335 Representing tables

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))))

(define (make-table)
  (list '*table*))

;;; p. 336 Two-dimensional tables

(define (lookup-2d key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert-2d! key-1 key-2 value table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'OK)

;;; p. 338 Creating local tables

(define (make-table-local)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'OK)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: table-local" m))))
    dispatch))

(define operation-table (make-table-local))
(define op-table-get (operation-table 'lookup-proc))
(define op-table-put! (operation-table 'insert-proc!))

;;; p. 341 ex. 3.27 Memoization

(define (memoize f)
  (let ((table (make-table-local)))
    (lambda (x)
      (let ((previously-computed-result
             ((table 'lookup-proc) 'dummy-key x)))
        (or previously-computed-result
            (let ((result (f x)))
              ((table 'insert-proc!) 'dummy-key x result)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))

;;; p. 357 Propagation of constraints (Celsius to Fahrenheit diagram)

;; w = 9
;; x = 5
;; y = 32
;; C * w = u
;; u = v * x
;; v + y = F

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'OK))

(define (run-temp-simulation)
  (celsius-fahrenheit-converter C F)
  (probe "Celsius temp" C)
  (probe "Fahrenheit temp" F)
  (set-value! C 25 'user))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1)) me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: adder" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: multiplier" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: constant" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value) (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request; probe" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'Ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'Ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: connector" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;;; p. 387 Implementing serializers

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))  ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell) #t (begin (set-car! cell #t) #f)))

;;; p. 396 Streams -- see streams.scm
;;; p. 443 Evaluator -- see evaluator.scm
