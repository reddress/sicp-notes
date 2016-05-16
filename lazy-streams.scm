;;; Enter these definitions in the driver loop of lazy-evaluator

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(define (display-first-ten sequence)
  (define (iter sequence count)
    (if (> count 10)
        'display-first-ten-done
        (begin (display (car sequence))
               (newline)
               (iter (cdr sequence) (+ count 1)))))
  (iter sequence 1))

;;; partial-sums
;; (1 2 3 4 5 6...)
;; (1 3 6 10 15 21...)

(define partial-sums
  (add-lists integers (cons 0 partial-sums)))

(define (display-partial-sums)
  (display-first-ten partial-sums))
