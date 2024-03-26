#lang racket/base

(provide configure)

(require (only-in unnwraith/at-exp/reader use-unnwraith-at-exp-readtable))

(define (configure data)
  (use-unnwraith-at-exp-readtable))
