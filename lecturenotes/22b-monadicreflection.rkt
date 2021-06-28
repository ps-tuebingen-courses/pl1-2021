#lang racket

(require racket/control)

(define-syntaxes (reify)
     (syntax-rules ()
       [(_ e)
        (reify-thunk (thunk e))]))

(define (return x) (list x))
(define (bind m f)
  (apply append (map f m)))

; Monadic reflection (Filinski)

(define (reflect m)
  (shift k (bind m k)))

(define (reify-thunk t)
  (reset (return (t))))

(reify (+ (reflect (list 1 2)) (reflect (list 3 4))))

; more information in "Monads in Action" by A. Filinski:
; https://doi.org/10.1145/1706299.1706354