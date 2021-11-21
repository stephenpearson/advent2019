#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (run mem pc)
  (let ([v1 (vector-ref mem pc)])
    (if (= v1 99)
      (vector-ref mem 0)
      (begin
        (let ([v2 (vector-ref mem (vector-ref mem (+ pc 1)))]
              [v3 (vector-ref mem (vector-ref mem (+ pc 2)))]
              [v4 (vector-ref mem (+ pc 3))])
          (vector-set! mem v4 (cond
            [(= v1 1) (+ v2 v3)]
            [(= v1 2) (* v2 v3)]
            [else v4]))
          (run mem (+ pc 4)))))))

(define orig (read-csv-of-numbers "input"))
(for* ([i (in-range 0 100)] [j (in-range 0 100)])
  (define prg (list->vector (apply list orig)))
  (vector-set! prg 1 i)
  (vector-set! prg 2 j)
  (when (= (run prg 0) 19690720)
    (printf "~a\n" (+ (* 100 i) j))))
