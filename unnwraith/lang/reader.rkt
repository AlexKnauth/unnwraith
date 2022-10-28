#lang racket/base

(provide read
         read-syntax
         get-info)

(require syntax/parse
         (prefix-in shrubbery: (submod shrubbery reader))
         "../groups-unnwraith-syntaxes.rkt")

(define get-info shrubbery:get-info)

(define (read in) (syntax->datum (read-syntax #f in)))

(define (read-syntax src in)
  (define stx (shrubbery:read-syntax src in))
  (syntax-parse stx
    #:datum-literals (module racket/base quote top)
    [(module name racket/base (quote (top input ...)))
     #:with (output ...) (groups-unnwraith-syntaxes (attribute input))
     (restx1 stx `(module ,#'name 'racket/base (quote (top ,@(attribute output)))))]
    [(module name lang (top input ...))
     #:with (output ...) (groups-unnwraith-syntaxes (attribute input))
     (restx1 stx `(module ,#'name ,#'lang ,@(attribute output)))]))

(define (restx1 src dat)
  (datum->syntax src dat src src))
