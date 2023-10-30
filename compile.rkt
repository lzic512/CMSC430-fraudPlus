#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = (Listof ID)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)
     (seq (Mov rax 0)
	  (Push rax)
     (compile-primn p es c))]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let x e1 e2)
     (compile-let1 x e1 e2 c c)]
    ;; TODO: implement let*, case, cond
    [(Let* xs es e)  (compile-let2 xs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

(define (compile-primn p es c)
  (match es
    ['() (seq (Pop r8))]
    [(cons a b)
    (seq (compile-e a (cons #f c))
	 (compile-op2 p)
	 (Push rax)
	 (compile-primn p b c))]))

;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c1 c2)
  (match x
    ['() (seq (compile-e e2 c1))]
    [(cons a rest1)
    (match e1
      [(cons b rest2)
      (seq (compile-e b c2)
      (Push rax)
      (compile-let1 rest1 rest2 e2 (cons a c1) c2)
      (Add rsp 8)
      )])]))


(define (compile-let2 x e1 e2 c)
  (match x
    ['() (seq (compile-e e2 c))]
    [(cons a rest1)
     (match e1
       [(cons b rest2)
        (seq (compile-e b c)
           (Push rax)
           (compile-let2 rest1 rest2 e2 (cons a c))
	   (Add rsp 8)
	   )])]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (compile-cond cs e z)
      (match cs
	['() 
	(let ((l1 (gensym 'cond)))
	(seq (compile-e e z)))]
	[(cons a res) 
	(match a
	[(Clause b c)
	(let ((l1 (gensym 'cond))
	(l2 (gensym 'cond)))
	(seq (compile-e b z)
	(Cmp 'rax val-false)
	(Je l1)
	(compile-e c z)
	(Jmp l2)
	(Label l1)
	(compile-cond res e z)
	(Label l2)
	))])]))


(define (compile-case e cs el z)
        (match cs
	['()
	(let ((l1 (gensym 'case)))
	(seq (compile-e el z)))]
	[(cons a res)
	(match a
	[(Clause b c)      
	(let ((l1 (gensym 'case))
	(l2 (gensym 'case)))
	(seq (compile-case-helper e b z)
	(Cmp 'rax val-false)
	(Je l1)
	(compile-e c z)
	(Jmp l2)
	(Label l1)
	(compile-case e res el z)
	(Label l2)
	))])]))

(define (compile-case-helper e lst z)
    (match lst
    ['() 
    (let ((l1 (gensym 'caseh)))
    (seq (Mov 'rax val-false)))]
    [(cons a res)
    (let ((l1 (gensym 'caseh))
    (l2 (gensym 'caseh)))
    (seq (compile-e e z)
    (Mov 'rcx 'rax)
    (compile-e a z)
    (Cmp 'rax 'rcx)
    (Je l1)
    (compile-case-helper e res z)
    (Jmp l2)
    (Label l1)
    (compile-e e z)
    (Label l2)
    ))]))
