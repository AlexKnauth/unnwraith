#lang unnwraith racket

require rackunit

define (traffic-light-next tl):
  cond
  | equal? tl "green"; "yellow"
  | equal? tl "yellow"; "red"
  | equal? tl "red"
    "green"
  | else
    error "bad"

define (describe-number l):
  match l
  | (list); "none"
  | list _
    "one"
  | list _ _; "two"
  | _; "many"

check-equal? (traffic-light-next "green") "yellow"
check-equal? (traffic-light-next "yellow") "red"
check-equal? (traffic-light-next "red") "green"

check-equal? (describe-number (list)) "none"
check-equal? (describe-number (list "a")) "one"
check-equal? (describe-number (list "a" "b")) "two"
check-equal? (describe-number (list "a" "b" "c")) "many"

