#lang unnwraith typed/racket

require:
  only-in typed/racket:
    #{:} #{::}
module+ test:
  require: typed/rackunit

x :: Number
define x 5

fact :: (Integer -> Integer)
define (fact n):
  if: zero? n
      1
      n * fact (n - 1)

module+ test:
  check-equal? (fact 5) 120

