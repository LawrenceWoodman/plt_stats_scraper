#lang racket
; PLaneT package stats scraper
;
; Copyright (C) 2013 Lawrence Woodman
; Licensed under an MIT licence.  Please see LICENCE.md for details.

(require "table_printer.rkt"
         "plt_scraper.rkt")

;======================
;    Configuration
;======================

; If you want to use a proxy uncomment the following line, enter
; the correct proxy (name port) and comment out the line: (define proxy #f)
;(define proxy '("localhost" 1234))
(define proxy #f)


;========================
;        Main
;========================

(define (plt-version-freq plt-details)
  (let ([plt-versions (hash-values plt-details)])
    (for/fold ([freq '#hash()]) ([plt-version plt-versions])
      (let ([major-version (exact-truncate plt-version)])
        (if (hash-has-key? freq major-version)
            (values (hash-update freq major-version add1))
            (values (hash-set freq major-version 1)))))))

(define (plt-stats plt-details)
  (let ([num-plts (length (hash-keys plt-details))])
    (for/fold ([stats '#hash()])
      ([(major-version freq) (plt-version-freq plt-details)])
      (let ([percentage (exact->inexact (/ freq num-plts))])
        (values (hash-set stats major-version
                          (list freq percentage)))))))


(define (print-stats stats)
  (let ([stats-entries
         (for/list ([(versions stat) stats])
           (let ([freq (number->string (first stat))]
                 [percentage (real->decimal-string (* 100 (second stat)) 1)])
             (list (number->string versions) freq percentage)))])
    (print-table '("Number of Major Versions"
                   "Number of Packages"
                   "Percentage of Packages") stats-entries)))


(let ([plt-details (scrape-planet proxy)])
  (print-stats (plt-stats plt-details)))