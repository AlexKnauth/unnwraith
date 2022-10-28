#lang racket/base

(provide groups-unnwraith-sexprs
         groups-unnwraith-syntaxes)

(require racket/contract
         racket/function
         racket/list
         racket/string
         racket/syntax
         syntax/parse
         syntax/srcloc
         syntax/stx)
(module+ test
  (require rackunit))

;; groups-unnwraith-sexprs : (Listof SExpr) -> (Listof SExpr)
(define/contract (groups-unnwraith-sexprs ss)
  (-> list? list?)
  (map syntax->datum
       (groups-unnwraith-syntaxes
        (map (curry datum->syntax #f) ss))))

;; groups-unnwraith-syntaxes : (Listof Syntax) -> (Listof Syntax)
(define/contract (groups-unnwraith-syntaxes ss)
  (-> (listof syntax?) (listof syntax?))
  (append-map group-unnwraith-syntaxes ss))

(define (restx1 src dat)
  (datum->syntax src dat src src))

(define-syntax-class op
  #:attributes (id)
  #:datum-literals (op)
  (pattern (op id)))

(define-syntax-class block
  #:datum-literals (block)
  (pattern (block group ...)))

(define-syntax-class alts
  #:datum-literals (alts)
  (pattern (alts block ...)))

(define-syntax-class simple-term
  (pattern {~not {~or :op :block :alts}}))

(define-syntax-class shmushable
  (pattern :id)
  (pattern :op)
  (pattern :keyword))

;; syntax-end : Syntax -> PosInt
(define (syntax-end s)
  (+ (syntax-position s) (syntax-span s)))

;; update-syntax-end : Syntax PosInt -> Syntax
(define (update-syntax-end a b-end)
  (update-source-location a
    #:span (- b-end (syntax-position a))))

(define (group-shmush s)
  (syntax-parse s
    #:datum-literals (group)
    [(group . as)
     (restx1 s (cons 'group (terms-shmush '() '() #f #'as)))]))

(define (terms-shmush before shmushing end after)
  (syntax-parse after
    [() #:when (empty? shmushing) before]
    [() (append before (list (shmushables-shmush shmushing end)))]
    [(a:shmushable . rst)
     (cond
       [(empty? shmushing)
        (terms-shmush before (list #'a) (syntax-end #'a) #'rst)]
       [(= end (syntax-position #'a))
        (terms-shmush before (append shmushing (list #'a)) (syntax-end #'a) #'rst)]
       [else
        (terms-shmush
         (append before (list (shmushables-shmush shmushing end)))
         (list #'a)
         (syntax-end #'a)
         #'rst)])]
    [(a . rst)
     (cond
       [(empty? shmushing)
        (terms-shmush (append before (list #'a)) '() #f #'rst)]
       [else
        (terms-shmush
         (append before (list (shmushables-shmush shmushing end) #'a))
         '()
         #f
         #'rst)])]))

(define (shmushables-shmush shmushing end)
  (syntax-parse shmushing
    [(a) #'a]
    [(:op ...+)
     (restx1
      (update-syntax-end (first shmushing) end)
      `(op ,(string->symbol (shmushables-string shmushing))))]
    [(:keyword . _)
     (restx1
      (update-syntax-end (first shmushing) end)
      (string->keyword (shmushables-string shmushing)))]
    [_
     (restx1
      (update-syntax-end (first shmushing) end)
      (string->symbol (shmushables-string shmushing)))]))

(define (shmushables-string shmushing)
  (string-append* (shmushable-first-string (first shmushing))
                  (map shmushable-rest-string (rest shmushing))))

(define (shmushable-first-string s)
  (syntax-parse s
    [x:id (symbol->string (syntax-e #'x))]
    [o:op (symbol->string (syntax-e #'o.id))]
    [k:keyword (keyword->string (syntax-e #'k))]))

(define (shmushable-rest-string s)
  (syntax-parse s
    [x:id (symbol->string (syntax-e #'x))]
    [o:op (symbol->string (syntax-e #'o.id))]
    [k:keyword (string-append "~" (keyword->string (syntax-e #'k)))]))

;; group-unnwraith-syntaxes : Syntax -> (Listof Syntax)
(define/contract (group-unnwraith-syntaxes s #:parens [parens #f])
  (->* (syntax?) (#:parens (or/c #f syntax?)) (listof syntax?))
  (shmushed-group-unnwraith-syntaxes (group-shmush s) #:parens parens))

;; shmushed-group-unnwraith-syntaxes : Syntax -> (Listof Syntax)
(define/contract (shmushed-group-unnwraith-syntaxes s #:parens [parens #f])
  (->* (syntax?) (#:parens (or/c #f syntax?)) (listof syntax?))
  (syntax-parse s
    #:datum-literals (group)
    [(group a:simple-term ...+ {~seq o:op b:simple-term ...+} ...+)
     #:with o-id (first (attribute o.id))
     (unless (for/and [(other (in-list (rest (attribute o.id))))]
               (free-identifier=? #'o-id other))
       (error 'bad))
     (list
      (restx1
       (or parens s)
       (cons
        #'o-id
        (append
         (shmushed-group-unnwraith-syntaxes (restx1 s (cons 'group (attribute a))))
         (append*
          (for/list [(b (in-list (attribute b)))]
            (shmushed-group-unnwraith-syntaxes (restx1 s (cons 'group b)))))))))]
    [(group a:simple-term ...)
     (group-unnwraith-results
      (map term-unnwraith-syntax (attribute a))
      #:parens parens
      #:group s)]
    [(group a:simple-term ... b:block)
     (group-unnwraith-results
      (append
       (map term-unnwraith-syntax (attribute a))
       (groups-unnwraith-syntaxes (attribute b.group)))
      #:parens parens
      #:group s)]
    [(group a:simple-term ... b:alts)
     (group-unnwraith-results
      (append
       (map term-unnwraith-syntax (attribute a))
       (alts-unnwraith-syntaxes #'b))
      #:parens parens
      #:group s)]))

;; alts-unnwraith-syntax : Syntax -> (Listof Syntax)
(define (alts-unnwraith-syntaxes s)
  (syntax-parse s
    #:datum-literals (alts)
    [(alts b:block ...)
     (map alt-unnwraith-syntax (attribute b))]))

;; alt-unnwraith-syntax : Syntax -> Syntax
(define (alt-unnwraith-syntax s)
  (syntax-parse s
    #:datum-literals (block)
    [(block a ...)
     (restx1 s (groups-unnwraith-syntaxes (attribute a)))]))

;; term-unnwraith-syntax : Syntax -> Syntax
(define/contract (term-unnwraith-syntax s)
  (-> syntax? syntax?)
  (syntax-parse s
    #:datum-literals (parens)
    [(parens) (restx1 s '())]
    [(parens a)
     (define lst (group-unnwraith-syntaxes #'a #:parens s))
     (unless (and (pair? lst) (empty? (rest lst)))
       (error 'bad))
     (first lst)]
    [(brackets g ...)
     (define lst (group-unnwraith-results
                  (groups-unnwraith-syntaxes (attribute g))
                  #:parens s))
     (unless (and (pair? lst) (empty? (rest lst)))
       (error 'bad))
     (first lst)]
    [_ s]))

;; group-unnwraith-results : (Listof Syntax) -> (Listof Syntax)
(define/contract (group-unnwraith-results rs #:parens [parens #f] #:group [group #f])
  (->* ((listof syntax?)) (#:parens (or/c #f syntax?) #:group (or/c #f syntax?))
       (listof syntax?))
  (cond
    [(and (not parens) (pair? rs)
          (or (empty? (rest rs)) (keyword? (syntax-e (first rs)))))
     rs]
    [else
     (list
      (restx1 (or parens group (update-syntax-end (first rs) (last rs)))
              rs))]))

(module+ test
  (check-equal?
   (syntax->datum
    (group-shmush
     #`(group
        #,(update-source-location #'x #:position 1 #:span 1)
        #,(update-source-location #'(op -) #:position 2 #:span 1)
        #,(update-source-location #'y #:position 3 #:span 1)
        #,(update-source-location #'z #:position 5 #:span 1))))
   '(group x-y z)))
