#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive
    [(PrimN p es) (interp-primn p es r 0)]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (interp-cond cs e r)]
    ;; TODO: implement case
    [(Case ev cs el) (interp-case (interp-env ev r) cs el r)]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let x e1 e2)
     (interp-let1 x e1 e2 r r)]
    ;; TODO: implement let*
    [(Let* xs es e) 
     (interp-let2 xs es e r)]))

;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

(define (interp-primn p es r m)
  (match es
    ['() m]
    [(cons a b)
     (interp-primn p b r (interp-prim2 p m (interp-env a r)))]))

(define (interp-let1 xs es e2 r1 r2)
  (match xs
    ['() (interp-env e2 r1)]
    [(cons a b)
     (match es 
     [(cons c d)
      (match (interp-env c r2)
       ['err 'err]
       [v (interp-let1 b d e2 (ext r1 a v) r2)])])]))

(define (interp-let2 xs es e2 r)
  (match xs
    ['() (interp-env e2 r)]
    [(cons a b)
     (match es
       [(cons c d)
        (match (interp-env c r)
       ['err 'err]
       [v (interp-let2 b d e2 (ext r a v))])])]))

(define (interp-cond es e r)
  (match es
    ['() (interp-env e r)]
    [(cons a b)
     (match a
       [(Clause c d) 
	(if (interp-env c r)
            (interp-env d r)
            (interp-cond b e r))])]))

(define (interp-case ev es e r)
  (match es
    ['() (interp-env e r)]
    [(cons a b)
     (match a
       [(Clause c d) 
	  (if (case-helper ev c r)
	      (interp-env d r)
	      (interp-case ev b e r))])]))

(define (case-helper e cs r)
  (if (member e (list-maker cs '() r))
      #t
      #f))

(define (list-maker cs es r)
  (match cs
    ['() es]
    [(cons a b)
     (list-maker b (cons (interp-env a r) es) r)]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))


;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

