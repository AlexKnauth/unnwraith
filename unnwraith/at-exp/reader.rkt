#lang racket/base

(provide make-unnwraith-at-exp-readtable
         unnwraith-at-exp-read
         unnwraith-at-exp-read-syntax
         wrap-reader
         use-unnwraith-at-exp-readtable
         (rename-out
          [unnwraith-at-exp-read read]
          [unnwraith-at-exp-read-syntax read-syntax])
         )

(require hygienic-reader-extension/extend-reader
         (for-meta -10 racket/base)
         (for-meta -9 racket/base)
         (for-meta -8 racket/base)
         (for-meta -7 racket/base)
         (for-meta -6 racket/base)
         (for-meta -5 racket/base)
         (for-meta -4 racket/base)
         (for-meta -3 racket/base)
         (for-meta -2 racket/base)
         (for-meta -1 racket/base)
         (for-meta 0 racket/base)
         (for-meta 1 racket/base)
         (for-meta 2 racket/base)
         (for-meta 3 racket/base)
         (for-meta 4 racket/base)
         (for-meta 5 racket/base)
         (for-meta 6 racket/base)
         (for-meta 7 racket/base)
         (for-meta 8 racket/base)
         (for-meta 9 racket/base)
         (for-meta 10 racket/base)
         (for-meta 11 (only-in racket/base #%app make-rename-transformer syntax))
         )

(define ch:command #\@)

(define (unnwraith-at-exp-read [in (current-input-port)])
  ((wrap-reader read) in))

(define (unnwraith-at-exp-read-syntax
         [src (object-name (current-input-port))]
         [in (current-input-port)])
  ((wrap-reader read-syntax) src in))

(define (wrap-reader p)
  (extend-reader p make-unnwraith-at-exp-readtable))

(define (make-unnwraith-at-exp-readtable
         [orig-rt (current-readtable)]
         #:command-char [command-char ch:command]
         #:outer-scope outer-scope)
  (define reader-proc (make-reader-proc orig-rt outer-scope))
  (define at-rt
    (make-readtable orig-rt
      command-char 'non-terminating-macro dispatcher)))

(define (use-unnwraith-at-exp-readtable [orig-rt (current-readtable)])
  (port-count-lines! (current-input-port))
  (current-readtable
   (make-unnwraith-at-exp-readtable orig-rt #:outer-scope values)))

(define ((make-reader-proc orig-rt outer-scope) char in src ln col pos)
  (error 'TODO))
