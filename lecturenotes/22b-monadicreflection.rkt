#lang racket

(require racket/control)

; Monadic style can encode continuation-passing style:
; just use the continuation monad

; but the converse is also true in a certain sense that was
; discovered by A. Filinski, whoh invented the notion of
; "monadic reflection". Here, we exemplify monadic reflection
; in terms of the list monad, but it is straightforward
; to abstract over the concrete monad and make this work
; for every monad.

; The list monad operations
(define (return x) (list x))
(define (bind m f)
  (apply append (map f m)))

; Monadic reflection (Filinski)

(define (reflect m)
  (shift k (bind m k)))

; this is a macro that transforms (reify e) into (reify-thunk (thunk e))
; its sole purpose is to prevent the evaluation of e and wrap it into a thunk
(define-syntaxes (reify)
     (syntax-rules ()
       [(_ e)
        (reify-thunk (thunk e))]))

(define (reify-thunk t)
  (reset (return (t))))

; Now we can write direct-style programs (in this case: a direct-style + function), yet use monadic features.
(reify (+ (reflect (list 1 2)) (reflect (list 3 4))))

; more information in "Representing Monads" by A. Filinski:
;https://doi.org/10.1145/174675.178047