#lang racket/base

(provide groups-unnwraith-sexprs
         groups-unnwraith-syntaxes)

(require racket/contract
         racket/function
         racket/list
         racket/match
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

(define (ooo-str? s)
  (and (string? s) (string-contains? s "...")))

(define-syntax-class op/shmushable
  #:attributes (id)
  #:datum-literals (op $ |#'|)
  (pattern (op {~and id {~not {~or $ |#'|}}})))

(define-syntax-class op/tight-prefix
  #:attributes (id)
  #:datum-literals (op $ |#'|)
  (pattern (op $) #:with id (restx1 this-syntax 'unsyntax))
  (pattern (op |#'|) #:with id (restx1 this-syntax 'quote)))

(define-syntax-class op
  #:attributes (id)
  (pattern :op/shmushable)
  (pattern :op/tight-prefix))

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
  (pattern :op/shmushable)
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
    [(:op/shmushable ...+)
     (define str (shmushables-string shmushing))
     (restx1
      (update-syntax-end (first shmushing) end)
      (cond
        [(ooo-str? str) (string->symbol str)]
        [else `(op ,(string->symbol str))]))]
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
    [o:op/shmushable (symbol->string (syntax-e #'o.id))]
    [k:keyword (keyword->string (syntax-e #'k))]))

(define (shmushable-rest-string s)
  (syntax-parse s
    [x:id (symbol->string (syntax-e #'x))]
    [o:op/shmushable (symbol->string (syntax-e #'o.id))]
    [k:keyword (string-append "~" (keyword->string (syntax-e #'k)))]))

;; group-tight-prefix-ops : Syntax -> Syntax
(define/contract (group-tight-prefix-ops s)
  (-> syntax? syntax?)
  (syntax-parse s
    #:datum-literals (group)
    [(group :op/tight-prefix _) s]
    [(group . as)
     (restx1 s (cons 'group (terms-tight-prefix-ops #'as)))]))

;; terms-tight-prefix-ops : List List ??? SyntaxList -> SyntaxList
(define (terms-tight-prefix-ops s)
  (syntax-parse s
    [() '()]
    [(o:op/tight-prefix . a-rst)
     (match-define (cons a rst) (terms-tight-prefix-ops #'a-rst))
     (define src (update-syntax-end #'o (syntax-end a)))
     (cons (restx1 src (list 'parens (restx1 src (list 'group #'o a))))
           rst)]
    [(a . rst)
     (cons #'a (terms-tight-prefix-ops #'rst))]))

;; group-unnwraith-syntaxes : Syntax -> (Listof Syntax)
(define/contract (group-unnwraith-syntaxes s #:parens [parens #f])
  (->* (syntax?) (#:parens (or/c #f syntax?)) (listof syntax?))
  (shmushed-group-unnwraith-syntaxes (group-shmush s) #:parens parens))

;; shmushed-group-unnwraith-syntaxes : Syntax -> (Listof Syntax)
(define/contract (shmushed-group-unnwraith-syntaxes s #:parens [parens #f])
  (->* (syntax?) (#:parens (or/c #f syntax?)) (listof syntax?))
  (syntax-parse (group-tight-prefix-ops s)
    #:datum-literals (group)
    [(group o:op a:simple-term ...+)
     (list
      (restx1
       (or parens s)
       (cons
        #'o.id
        (shmushed-group-unnwraith-syntaxes
         (restx1 s (cons 'group (attribute a)))))))]
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
      #:group s)]
    [(group a:simple-term ... b1:block b2:alts)
     (group-unnwraith-results
      (append
       (map term-unnwraith-syntax (attribute a))
       (groups-unnwraith-syntaxes (attribute b1.group))
       (alts-unnwraith-syntaxes #'b2))
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
    #:datum-literals (parens brackets quotes)
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
    [(quotes a)
     (define lst (group-unnwraith-syntaxes #'a))
     (unless (and (pair? lst) (empty? (rest lst)))
       (error 'bad))
     (restx1 s (list 'quasisyntax (first lst)))]
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
    (shmushables-shmush
     (list
      (update-source-location (quote-syntax (op ...)) #:position 3 #:span 3))
     6))
   '...)

  (check-equal?
   (syntax->datum
    (group-shmush
     #`(group
        #,(update-source-location #'x #:position 1 #:span 1)
        #,(update-source-location (quote-syntax (op ...)) #:position 3 #:span 3)
        #,(update-source-location #'y #:position 7 #:span 1)
        #,(update-source-location #'z #:position 9 #:span 1))))
   '(group x ... y z))
  
  (check-equal?
   (syntax->datum
    (group-shmush
     #`(group
        #,(update-source-location #'x #:position 1 #:span 1)
        #,(update-source-location #'(op -) #:position 2 #:span 1)
        #,(update-source-location #'y #:position 3 #:span 1)
        #,(update-source-location #'z #:position 5 #:span 1))))
   '(group x-y z))

  (check-equal?
   (syntax->datum
    (group-shmush
     #`(group
        #,(update-source-location #'x #:position 1 #:span 1)
        #,(update-source-location #'(op $) #:position 2 #:span 1)
        #,(update-source-location #'y #:position 3 #:span 1)
        #,(update-source-location #'z #:position 5 #:span 1))))
   '(group x (op $) y z))

  (check-equal?
   (syntax->datum
    (group-shmush
     #`(group
        #,(update-source-location #'x #:position 1 #:span 1)
        #,(update-source-location #'(op |#'|) #:position 2 #:span 2)
        #,(update-source-location #'y #:position 4 #:span 1)
        #,(update-source-location #'z #:position 6 #:span 1))))
   '(group x (op |#'|) y z))

  (check-equal?
   (syntax->datum
    (group-tight-prefix-ops #`(group x (op |#'|) y z)))
   '(group x (parens (group (op |#'|) y)) z)))
