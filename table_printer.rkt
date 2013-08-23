#lang racket
(require math/base)

(provide print-table)

(define (print-table-headings column-names column-widths)
  (let ([table-width
         (+ (sum column-widths) (* 3 (length column-names)) 1)])
    (printf "| ~a | " (first column-names))
    (for ([column-name (rest column-names)])
      (printf "~a | " column-name))
    (printf "~n~a~n" (~a "" #:width table-width #:pad-string "-"))))

(define (print-table-entries entries column-widths)
  (let ([max-value-widths
         (for/fold ([widths '#hash()]) ([row entries])
           (for/fold ([widths widths])
             ([value row] [column-num (in-naturals)])
             (let ([recorded-width (hash-ref widths column-num 0)])
               (values (hash-set widths column-num
                                 (max (string-length value)
                                      recorded-width))))))])
    (for ([row entries])
      (for ([column-value row]
            [column-width column-widths]
            [(col-num max-value-width) max-value-widths])
        (printf "| ~a "
                (~a (~a column-value #:width max-value-width #:align 'right)
                    #:align 'center #:width column-width)))
      (printf "|~n"))))


;==============================
;     Exported Functions
;==============================

(define (print-table column-names entries)
  (let ([column-widths
         (for/list ([column-name column-names])
           (string-length column-name))])
    (print-table-headings column-names column-widths)
    (print-table-entries entries column-widths)))