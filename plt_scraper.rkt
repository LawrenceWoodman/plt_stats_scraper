#lang racket/base
; PLaneT package details scraper
;
; Copyright (C) 2013 Lawrence Woodman
; Licensed under an MIT licence.  Please see LICENCE.md for details.

(require racket/list
         net/url
         (planet neil/html-parsing:2:0)
         (planet clements/sxml2:1:=3))

(provide scrape-planet)

(define planet-url (string->url "http://planet.racket-lang.org")) 

; Download the PLaneT index and convert it to xexp
(define (get-planet-ns planet-url)
  (call/input-url planet-url get-pure-port html->xexp))

; Find those nodesets that superficially look like plt entries
(define (possible-plts planet-ns)
  (let ([tr-filledin ((sxpath '(// (tr (@ (equal? (class "filledin"))))))
                      planet-ns)])
    (if (empty? tr-filledin)
        (error "Can't find any tr class=filledin elements")
        tr-filledin)))

; Find just those nodesets that conform to plt entries
(define (just-plts planet-ns)
  (let ([plts ((sxml:filter (sxpath '((td 3)))) (possible-plts planet-ns))])
    (if (empty? plts)
        (error "Can't find anything that looks like a plt")
        plts)))


(define (plt-names plt-nss)
  (for/list ([name-node ((sxpath '((td 1) a)) plt-nss)])
    (first (sxml:content name-node))))

(define (plt-versions plt-nss)
  (for/list ([version-node ((sxpath '((td 2))) plt-nss)])
    (version-ns->real (sxml:content version-node))))

(define (version-ns->real v-node)
  (let* ([major-v (first (regexp-match #px"[[:digit:]]+"
                                       (first ((node-pos 1) v-node))))]
         [minor-v (first (regexp-match #px"[[:digit:]]+" 
                                       (first ((node-pos 3) v-node))))]
         [minor-v-digits (string-length minor-v)])
    (+ (string->number major-v) (/ (string->number minor-v)
                                   (expt 10.0 minor-v-digits)))))

(define (join-names-to-versions names versions)
  (for/hash ([name names] [version versions])
    (values name version)))

;==============================
;     Exported functions
;==============================
(define (scrape-planet [proxy #f])
  (when proxy (current-proxy-servers `(("http" ,(first proxy)
                                               ,(second proxy)))))
  
  (let ([plt-nss (just-plts (get-planet-ns planet-url))])
    (let ([plt-names (plt-names plt-nss)]
          [plt-versions (plt-versions plt-nss)])
      (join-names-to-versions plt-names plt-versions))))
