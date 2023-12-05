#lang racket #| * CSC324H5 Fall 2023: Assignment 4 * |#
#|
Module:        a4
Description:   Assignment 4: A CPSer and A Relational CPSer
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2023
|#

; This specifies which functions this module exports. Don't change this!
(provide cps-define cps-insert cps-expr cps-inserto cps-expro)

(require "mk.rkt") ; for task 2

;-------------------------------------------------------------------------------
; * Task 1: A CPSer *
;-------------------------------------------------------------------------------

#|
(cps-define def) -> list?
  def: list?
    A definition that follows the grammar for <def> form the handout

  Returns the equivalent definition in CPS, following the grammar
  for <cdef> from the handout.
|#
(define/match (cps-define def)
  [(`(define ,name ,expr)) ; this pattern uses the quasi-quote ` which
                           ; works the similarly as the quote operation '
                           ; but allows for unescaping quotation with the
                           ; comma , --- so that `(define ,name ,expr)
                           ; is equivalent to (list 'define name expr)
   `(define ,name ,(cps-expr expr '(_)))])
    ; likewise, this above code is equivalent to
    ; (list 'define name (cps-expr expr '(_)))


#|
(cps-insert t frames) -> (and/or number? symbol? list?) 
  t: (and/or number? symbol? list?) 
    A trivial expression defined by the grammar for <t> from the handout
  frames: (list?) 
    A stack of frames where the first element of the list represents the
    top of the stack. Each frame is an defined by the grammar for <frame>
    from the handout.

  Returns a <cexpr>.
|#
(define/match (cps-insert t frames)
  [(t frames) (void)])


#|
(cps-expr expr frames) -> (and/or number? symbol? list?) 
  expr: (and/or number? symbol? list?) 
    An expression defined by the expression <expr> from the handout, to be
    transformed into continuation passing style
  frames: (list?) 
    A stack of frames where the first element of the list represents the
    top of the stack. Each frame is an defined by the grammar for <frame>
    from the handout.

  Returns the CPSed version of expr, following the grammar for <cexpr> in
  the handout.
|#
(define/match (cps-expr expr frames)
  [(expr frames) (void)])

;-------------------------------------------------------------------------------
; * Task 1: A Relational CPSer *
;-------------------------------------------------------------------------------


#|
(cps-expro expr frames cexpr)
  expr: (and/or number? symbol? list?) 
    An expression defined by the expression <expr> from the handout, to be
    transformed into continuation passing style
  frames: (list?) 
    A stack of frames where the first element of the list represents the
    top of the stack. Each frame is an defined by the grammar for <frame>
    from the handout.
  cexpr: (and/or number? symbol? list?) 
    The CPSed version of expr, following the grammar for <cexpr> in
    the handout.

  The relational form of the `cps-expr` function.
|#
(define (cps-expro expr frame cexpr)
  (conde
    ((conde ((symbolo expr)) ((numbero expr)))
     (cps-inserto expr frame cexpr))
    ((fresh (op e1 e2)
       (== expr `(,op ,e1 ,e2))
       (opo op)
       (cps-expro e1 `((,op _ ,e2) . ,frame) cexpr)))
    ((fresh (fn arg)
       (== expr `(,fn ,arg))
       (cps-expro fn `((app _ ,arg) . ,frame) cexpr)))
    ((fresh (condition then alt)
       (== expr `(if ,condition ,then ,alt))
       ; TODO: what call(s) go(es) here?
       ))
    ((fresh (id body k cpsbody)
       (== expr `(lambda (,id) ,body))
       (cps-inserto `(lambda (,id ,k) ,cpsbody) frame cexpr)
       ; TODO: what call(s) go(es) here?
       ; notice that instead of using a call to `gensym`, we
       ; use a fresh logic variable `k` to represent the
       ; continuation variable
       ))
))


#|
(cps-inserto t frames cexpr) -> (and/or number? symbol? list?) 
  t: (and/or number? symbol? list?) 
    A trivial expression defined by the grammar for <t> from the handout
  frames: (list?) 
    A stack of frames where the first element of the list represents the
    top of the stack. Each frame is an defined by the grammar for <frame>
    from the handout.
  cexpr: (and/or number? symbol? list?) 
    The CPSed expression, following the grammar for <cexpr> in the handout.

  The relational form of the `cps-insert` function.
|#
(define (cps-inserto cin frame cout)
  (void))


; Helper function
(define (opo op)
   (conde ((== op '=)) ((== op '+)) ((== op '-))))

