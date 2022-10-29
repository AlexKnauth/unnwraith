#lang unnwraith racket

require: syntax/parse
         rackunit

define stx1 'a b c'

syntax-parse stx1
| x y z
  check-equal? (symbol->string (syntax-e 'x')) "a"
  check-equal? (symbol->string (syntax-e 'y')) "b"
  check-equal? (symbol->string (syntax-e 'z')) "c"

define stx2 'd e ($ stx1)'

syntax-parse stx2
| u v w
  check-equal? (symbol->string (syntax-e 'u')) "d"
  check-equal? (symbol->string (syntax-e 'v')) "e"
  check-equal? 'w' stx1

