#lang racket/base
(require racket/contract
         racket/list
         racket/format
         math/base)

(provide (contract-out
          [print-table (-> (listof string?)
                           (listof (listof string?))
                           any/c)]))

(define (print-table-headings column-names)
  (printf "| ~a | " (first column-names))
  (for ([column-name (rest column-names)])
    (printf "~a | " column-name))
  (printf "~n"))

(define (print-table-separator column-widths)
  (printf "+~a+" (make-string (+ 2 (first column-widths)) #\-))
  (for ([column-width (rest column-widths)])
    (printf "~a+" (make-string (+ 2 column-width) #\-)))
  (printf "~n"))

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
    (print-table-separator column-widths)
    (print-table-headings column-names)
    (print-table-separator column-widths)
    (print-table-entries entries column-widths)
    (print-table-separator column-widths)))