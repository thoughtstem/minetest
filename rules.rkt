#lang racket

(require "core.rkt")

(provide add-behaviour-to)
(provide game-rule)

(require (for-syntax racket/syntax))

(define-syntax (add-behaviour-to stx)
  (syntax-case stx ()
    [(_ target (key val)) 
     (with-syntax* ([target-id (format-id stx "~a" #'target)]
                    [key-str (symbol->string
                              (format-symbol "~a" #'key))])
       #`(add-behaviour target-id
                       (list key-str val)
                       my-mod))]))


(require (for-syntax racket))
(define-syntax (game-rule stx)
  (syntax-case stx (on: do:)
    [(_ on: kind verb subj do: effects ...)
            (with-syntax* ([subj-id (format-id stx "~a" #'subj)]
                           [callback-id (symbol->string
                                         (format-symbol "on-~a" #'verb))]
                           [sequence-id (format-id stx "on-~a-~a-sequence" #'kind #'verb)]
                           [effect-prefix (symbol->string (format-symbol "on-~a-~a" #'kind #'verb))]
                           [(prefixed-effects ...) (map (lambda (x)
                                                    (cons
                                                     
                                                      (format-id stx "~a-~a"
                                                              (syntax->datum #'effect-prefix)
                                                              (syntax->datum (first (syntax->list x))))
                                                     (rest (syntax->list x))))
                                                  (syntax->list #'(effects ...)))])
              
               #`(add-behaviour subj-id
                               (list callback-id (sequence-id
                                                   prefixed-effects ...
                                                   ))
                               my-mod))]))

