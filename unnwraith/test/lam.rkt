#lang unnwraith racket

require syntax/parse
module+ test:
  require rackunit

struct Val [v] ~transparent
struct Var [x] ~transparent
struct App [f, a] ~transparent
struct Lam [x, b] ~transparent
struct If [i, t, e] ~transparent

define (parse stx):
  syntax-parse stx:
    ~datum-literals [λ, if]
  | n
    ~declare n number
    Val (syntax-e 'n')
  | s
    ~declare s string
    Val (syntax-e 's')
  | x
    ~declare x id
    Var (symbol->string (syntax-e 'x'))
  | λ x: b
    ~declare x id
    Lam (symbol->string (syntax-e 'x')) (parse 'b')
  | (if i | t | e)
    If (parse 'i') (parse 't') (parse 'e')
  | f a
    App (parse 'f') (parse 'a')
  | f a ...+ b
    App (parse 'f a ...') (parse 'b')
    

define (cmp exp):
  match exp
  | Val v; λ (env): v
  | Var x
    λ (env): hash-ref env x
  | App f a
    define fc: cmp f
    define ac: cmp a
    λ (env): (fc env) (ac env)
  | Lam x b
    define bc: cmp b
    λ (env): λ (xv): bc (hash-set env x xv)
  | If i t e
    define ic: cmp i
    define tc: cmp t
    define ec: cmp e
    λ (env): if (ic env) (tc env) (ec env)

define ((ntp exp) env):
  match exp
  | Val v; v
  | Var x; hash-ref env x
  | App f a; ((ntp f) env) ((ntp a) env)
  | Lam x b; λ (xv): ((ntp b) (hash-set env x xv))
  | If i t e; if ((ntp i) env) ((ntp t) env) ((ntp e) env)

module+ test:
  define-check (check-lam env stx v):
    define exp: parse stx
    check-equal? ((ntp exp) env) v
    check-equal? ((cmp exp) env) v

module+ test:
  define std:
    hash:
      "+"; λ (x): λ (y): x + y
      "-"; λ (x): λ (y): x - y
      "*"; λ (x): λ (y): x * y
      "/"; λ (x): λ (y): x / y
      "zero?"; zero?
  check-lam std '1' 1
  check-lam std '1 + 2' 3
  check-lam std '2 * 3' 6
  check-lam std '10 - 3' 7
  check-lam std '27 / 3' 9
  check-lam std:
    '(λ Y:
         (λ fact:
            fact 5):
           Y (λ f: λ x:
                     if (zero? x)
                     | 1
                     | x * f (x - 1))):
       λ fof:
         (λ fos: fos fos):
           λ fos: fof (λ x: fos fos x)'
    120
  check-lam std:
    '(λ Y:
         (λ fib:
            fib 12):
           Y (λ f: λ x:
                     if (zero? x)
                     | 0
                     | if (zero? (x - 1))
                       | 1
                       | f (x - 2) + f (x - 1))):
       λ fof:
         (λ fos: fos fos):
           λ fos: fof (λ x: fos fos x)'
    144
