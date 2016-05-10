;;; silly code

(define (pseudo-chinese-name)
  (let ((initials '("b"
                    "c"
                    "ch"
                    "d"
                    "f"
                    "g"
                    "h"
                    "j"
                    "k"
                    "l"
                    "m"
                    "n"
                    "p"
                    "q"
                    "r"
                    "sh"
                    "t"
                    "w"
                    "y"
                    "z"
                    "zh"))
        (finals '("an"
                  "ang"
                  "ao"
                  "ai"
                  "eh"
                  "en"
                  "eng"
                  "i"
                  "ie"
                  "iou"
                  "iu"
                  "iau"
                  "iao"
                  "ing"
                  "in"
                  "ien"
                  "o"
                  "ou"
                  "ong"
                  "on"
                  "u"
                  "ü"
                  "ung"
                  "un")))
    ;;; http://stackoverflow.com/questions/4583224/how-do-i-write-a-procedure-that-randomly-selects-a-pair-from-a-list
    (define (select-random lst)
      (let ((len (length lst)))
        (list-ref lst (random len))))
    (define (string-upcase-first s)
      (let ((char-list (string->list s)))
        (apply string
         (cons (char-upcase (car char-list))
               (cdr char-list)))))
    (define (string-titlecase s)
      (string-join 
       (map string-upcase-first (string-split s #\space))))
    (define (syllable)
      (string-append (select-random initials) (select-random finals)))
    (define (first-name)
      (string-append (syllable) (syllable)))
    (define (full-name)
      (string-titlecase
       (string-append (syllable) " " (syllable) (syllable))))
    (full-name)))
