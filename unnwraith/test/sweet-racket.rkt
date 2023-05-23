#lang unnwraith racket

module+ test:
  require: rackunit

define (fact n):
  if: zero? n
      1
      n * (fact (n - 1))

module+ test:
  check-equal? (fact 5) 120
  check-equal? (fact 6) 720
  check-equal? (fact 7) 5040

define (fib n):
  match n
  | 0; 0
  | 1; 1
  | n
    fib (n - 2) + fib (n - 1)

module+ test:
  check-equal? (fib 12) 144
  check-equal? (fib 16) 987

define-syntax-rule (foo x ...): x + ...

module+ test:
  check-equal? (foo 1 2 3) 6

define-syntax-rule:
  bar x ...
  #{+} x ...

module+ test:
  check-equal? (bar 1 2 3) 6

define clf:
  case-lambda
  | (); 0

module+ test:
  check-equal? (clf) 0

define (fibfast n):
  if: n < 2 
      n
      fibup n 2 1 0

define (fibup max count n-1 n-2):
  if: max = count
      n-1 + n-2
      fibup max (count + 1) (n-1 + n-2) n-1

module+ test:
  check-equal? (fibfast 9) 34

define-syntax get-paren-shape:
  lambda (stx):
    '$(or (syntax-property stx #'paren-shape) #{#\(})'

module+ test:
  check-equal? (get-paren-shape) #{#\(}
  check-equal? [get-paren-shape] #{#\[}

