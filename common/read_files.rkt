#lang racket

(provide read-file-of-numbers)
(provide read-csv-of-numbers)

(define (read-file-lines filename)
  (let ([in (open-input-file filename)])
    (string-split (port->string in #:close? #t))))

(define (read-file-of-numbers filename)
  (map string->number (read-file-lines filename)))

(define (read-csv-of-numbers filename)
  (map string->number (string-split (car (read-file-lines filename)) #rx"[,\n]")))
