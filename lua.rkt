#lang racket

(provide define-lua)
(provide lua)
(provide lua-code)
(provide compile-lua-defs)

(require "core.rkt")
(require (for-syntax racket/syntax))

(struct lua (name code) #:transparent)

(define (lua-call code)
  (lua "" code))

(define/contract (compile-lua-def m d)
  (-> mod-struct? lua? string?)
   (++ "-- Some lua code sent by Racket\n"
       "function " (lua-name d) "()\n"
       (lua-code d)
       "end\n"))

(define/contract (export-lua-def-code m i)
  (-> mod-struct? lua? boolean?)
     (with-output-to-file (lua-file-for m) #:exists 'append 
       (lambda () (printf (++
                           (compile-lua-def m i)
                           "\n"))))
     #t)

(define/contract (compile-lua-defs m defs)
  (-> mod-struct? (listof lua?) boolean?)
  (all-true (map (curry export-lua-def-code m) defs)))

(define-syntax (define-lua stx)
  (syntax-case stx ()
    [(_ id body-string)
            (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
              #`(begin
                  (define id (lua name body-string) )
                  (set-my-mod! (add-lua-def my-mod id))
                  ) ) ]))

