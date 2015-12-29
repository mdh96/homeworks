#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "hw2.rkt")

(define (repeat f n)
  (if (= n 0)
      '()
      (let ([x (f)])
        (cons x (repeat f (- n 1))))))

(define (square x)
  (* x x))

(define hw2-tests
  (test-suite
   "Homework 2 Tests"
   (check-equal? (let ([next (iterator '(0 2 7))])
                   (repeat next 5))
                 '(0 2 4 6 ())
                 "(repeat next 5)")
   (check-equal? (stream->list (stream-seq square '(1 1 0)))
                 '()
                 "(stream->list (stream-seq square '(1 1 0)))")
   (check-equal? (stream-first (stream-rest (stream-seq square '(1 1 7))))
                 4
                 "((stream-first (stream-rest (stream-seq square '(1 1 7))))")
   (check-equal? (scan + 0 '())
                 '(0)
                 "(scan + 0 '())")
   (check-equal? (scan + 0 '(1 2 3 4 5 6))
                 '(0 1 3 6 10 15 21)
                 "(scan + 0 '(1 2 3 4 5 6))")
   (check-equal? (scan * 1 '(1 2 3 4 5 6))
                 '(1 1 2 6 24 120 720)
                 "(scan * 1 '(1 2 3 4 5 6))")
   (check-equal? (stream->list (stream-scan + 0 empty-stream))
                 '(0)
                 "(stream->list (stream-scan + 0 empty-stream))")
   (check-equal? (stream->list (stream-scan + 0 (stream 1 2 3 4 5 6)))
                 '(0 1 3 6 10 15 21)
                 "(stream->list (stream-scan + 0 (stream 1 2 3 4 5 6)))")
   (check-equal? (stream->list (stream-scan * 1 (stream 1 2 3 4 5 6)))
                 '(1 1 2 6 24 120 720)
                 "(stream->list (stream-scan * 1 (stream 1 2 3 4 5 6)))")
   (check-equal? (let ([assocs '((ben "short") (cara "walking") (dan "bald"))])
                   (lookup 'albert assocs))
                 '()
                 "(lookup 'albert assocs))")
   (check-equal? (let ([assocs '((ben "short") (cara "walking") (dan "bald"))])
                   (lookup 'ben assocs))
                 '(ben "short")
                 "(lookup 'ben assocs))")
))

(run-tests hw2-tests)