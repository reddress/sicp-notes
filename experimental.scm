;;; accumulate list of divisors

(define (get-divisors n)
  (define (get-divisors-iter n divisors)
    (let ((sd (smallest-divisor n)))
      (if (= n 1)
          (reverse divisors)
          (get-divisors-iter (/ n sd) (cons sd divisors)))))
  (get-divisors-iter n (list)))

;;; Wikipedia
;;; Ramanujan Journal of Indian Mathematical Society problem
;;; sqrt(1 + 2 * sqrt(1 + 3 * sqrt(1 + ...)))

;;; (sqrt (+ 1 (* 2 (sqrt (+ 1 (* 3 (sqrt (+ 1 (* 4 ...

(define (inf-sqrt up-to)
  (define (inf-sqrt-iter up-to i)
    (if (= i up-to)
        0
        (sqrt (+ 1 (* i (inf-sqrt-iter up-to (+ i 1)))))))
  (inf-sqrt-iter up-to 2))

;;; Answer: 3

;;; ref. p. 103

(define (repeatedly-call f x n)
  (if (<= n 1)
      (f x)
      (f (repeatedly-call f x (- n 1)))))

;;; p. 143 Ex. 2.18

(define (hc-reverse lst)
  (define (hc-reverse-iter lst result)
    (if (null? lst)
        result
        (hc-reverse-iter (cdr lst) (cons (car lst) result))))
  (hc-reverse-iter lst '()))

(define (hc-upcase-first-char word)
  (let ((chars (string->list word)))
    (list->string (cons (char-upcase (car chars)) (cdr chars)))))

;;; R5RS Iteration

;; countdown
(do ((i 5 (- i 1)))
    ((= i 0) 'Blast-off!)
  (display i)
  (newline))

(let loop ((i 5))
  (cond ((= i 0) 'Blast-off!)
        (else (display i)
              (newline)
              (loop (- i 1)))))

;;; generalized map for multiple lists, Exercise 3.50

(define (my-map-mult proc . args)
  (if (null? (car args))
      '()
      (cons
       (apply proc (map car args))
       (apply my-map-mult
              (cons proc (map cdr args))))))

;;; finite ex. 3.55

;; 0 1 2 3 4 5   ; add 0, 1

;; 1 (2 3 4 5)   ; add 1, 2
;; 1 3 (3 4 5)   ; add 3, 3
;; 1 6 (4 5)     ; add 6, 4

(define (partial-sums-finite integers last count)
  (if (> count 10)
      'done
      (if (null? integers)
          '()
          (let ((head (+ last
                         (car integers))))
            (cons head (partial-sums-finite (cdr integers) head (+ count 1)))))))

(define (test-partial-sums-finite)
  (partial-sums-finite '(1 2 3 4 5 6 7 8 9 10) 0 0))
