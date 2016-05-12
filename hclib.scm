;;; Custom functions, all prefixed with hc-
;;; Using GNU Guile

(use-syntax (ice-9 syncase))

(define (hc-symbol-concat . syms)
  (let ((strs (map symbol->string syms)))
    (string->symbol (apply string-append strs))))

(define (hc-field-to-full-spec type-name field-name)
  (list field-name
        (symbol-concat type-name '- field-name)
        (symbol-concat 'set- type-name '- field-name '!)))

;;; Define a generic record type
;;; Identifiers are not quoted
;;;
;;; (hc-record-type student name age)
;;; (define john (make-student "John" 19))
;;; Getter: (student-age john)
;;; Setter: (set-student-age! john 20)
(define-macro (hc-record-type type-name . field-names)
  `(define-record-type ,type-name
     (,(hc-symbol-concat 'make '- type-name) ,@field-names)
     ,(hc-symbol-concat type-name '?)
     ,@(map
        (lambda (field-name) (hc-field-to-full-spec type-name field-name))
        field-names)))

;;; Execute body n times 
;;; (hc-repeat n body)
;;;
;;; (hc-repeat 3 (display "Hello!") (newline))
(define-syntax hc-repeat
  (syntax-rules ()
    ((repeat n body ...)
     (do ((j 1 (+ j 1)))
         ((> j n))
       body ...))))

(define (hc-map-to-words f sentence)
  (string-join
   (map f (string-split sentence #\space))))

(define (hc-upcase-first-char word)
  (let ((chars (string->list word)))
    (list->string (cons (char-upcase (car chars)) (cdr chars)))))

(define (hc-reverse-string str)
  (list->string (reverse (string->list str))))
