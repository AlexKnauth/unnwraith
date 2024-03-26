#lang lang-extension
#:lang-extension unnwraith-at-exp make-unnwraith-at-exp-lang-reader
#:lang-reader unnwraith-at-exp-lang
(require lang-reader/lang-reader
         (only-in "../reader.rkt" wrap-reader))

(define (make-unnwraith-at-exp-lang-reader lang-reader)
  (define/lang-reader [-read -read-syntax -get-info] lang-reader)
  (make-lang-reader
   (wrap-reader -read)
   (let ([read-syntax (wrap-reader -read-syntax)])
     (lambda args
       (define stx (apply read-syntax args))
       (define old-prop (syntax-property stx 'module-language))
       (define new-prop
         `#(unnwraith/at-exp/lang/language-info get-language-info ,old-prop))
       (syntax-property stx 'module-language new-prop)))
   -get-info))
