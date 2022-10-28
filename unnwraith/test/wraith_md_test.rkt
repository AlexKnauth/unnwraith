#lang racket/base

(require racket/runtime-path
         syntax/parse
         rackunit
         lang-file/read-lang-file
         "../groups-unnwraith-syntaxes.rkt")

(define-runtime-path wraith_md_actual.rkt "wraith_md_actual.rkt")
(define-runtime-path wraith_md_expected.rktd "wraith_md_expected.rktd")

;; inputs : (Listof Syntax)
(define inputs
  (syntax-parse (read-lang-file wraith_md_actual.rkt)
    #:datum-literals (module racket/base quote top)
    [(module _ racket/base (quote (top input ...)))
     (attribute input)]))

;; actuals : (Listof SExpr)
(define actuals (groups-unnwraith-sexprs inputs))

;; expecteds : (Listof SExpr)
(define expecteds (read (open-input-file wraith_md_expected.rktd)))

(for [(actual (in-list actuals))
      (expected (in-list expecteds))]
  (check-equal? actual expected))

(check-equal? (length actuals) (length expecteds))
