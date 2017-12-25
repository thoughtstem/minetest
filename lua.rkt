#lang racket

(provide define-lua)
(provide call-lua)

(require "core.rkt")
(require (for-syntax racket/syntax))

;Basically a thunk in lua
(define (lua-def id code m)
  (asset-struct id ""
                (++ "function " id "()\n"
                    code
                    "\nend\n")
                m))

(define (call-lua ref)
  (special-compile
   (thunk
    (format "~a()" (asset-struct-name ref)))))


(define-syntax (define-lua stx)
  (syntax-case stx ()
    [(_ id body-string)
            (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
              #`(begin
                  (define id (lua-def name body-string my-mod) )
                  (set-my-mod! (add-lua-def my-mod id))
                  ) ) ]))

