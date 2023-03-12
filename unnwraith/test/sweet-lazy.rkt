#lang unnwraith lazy

module+ test:
  require: rackunit

define fibs:
  cons 0: cons 1: map #{+} fibs (cdr fibs)

module+ test:
  check equal? (list-ref fibs 8) 21
  check equal? (list-ref fibs 12) 144
  check equal? (list-ref fibs 16) 987

define (fib n):
  cond
  | n = 0; 0
  | n = 1; 1
  | else
    fib (n - 2) + fib (n - 1)

module+ test:
  check equal? (fib 8) 21
  check equal? (fib 12) 144
  check equal? (fib 16) 987

define (fact n):
  if: zero? n
      1
      n * (fact (n - 1))

module+ test:
  check equal? (fact 5) 120
  check equal? (fact 6) 720
  check equal? (fact 7) 5040

