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
