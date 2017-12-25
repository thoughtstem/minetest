#lang racket

(provide define-recipe)

(require (for-syntax racket/syntax))

(require 2htdp/image)
(require "core.rkt")

;minetest.register_craft({
;    type = "shapeless",
;    output = "mymod:diamond",
;    recipe = {"mymod:diamond_fragments", "mymod:diamond_fragments", "mymod:diamond_fragments"}
;})
(define (recipe-struct id desc num output inputs m)
  (asset-struct id
                desc
                (make-immutable-hash
                 (list
                  (cons 'type (if (list? (first inputs)) "shaped" "shapeless"))
                  (cons 'output (format "~a ~a"
                                        (asset-name output)
                                        num))
                  (cons 'recipe (tree-map asset-name inputs))
                  ))
                m))


;(define (compile-recipe-input-shaped m r)
;  (let ([asset-names (tree-map
;                      (curry asset-name m)
;                      (recipe-struct-inputs r))])
;    (format "recipe = {~a}" (string-join
;                               (map (curryr compile-arr STR_TYPE) asset-names)
;                               ",\n"))))

(define-syntax (define-recipe stx)
  (syntax-case stx (make: from:)
    [(_ id make: num output from: (i1 i2 i3) (i4 i5 i6) (i7 i8 i9) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (asset-struct name "" num output (list (list i1 i2 i3)
                                                              (list i4 i5 i6)
                                                              (list i7 i8 i9))
                                    my-mod))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: (i1 i2) (i3 i4) (i5 i6) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2)
                                                              (list i3 i4)
                                                              (list i5 i6))
                                     my-mod))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: (i1 i2 i3) (i4 i5 i6) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2 i3)
                                                              (list i4 i5 i6))
                                     my-mod))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: (i1 i2) (i3 i4) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2)
                                                              (list i3 i4))
                                     my-mod))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: items ... ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list items ...) my-mod))
           (set-my-mod! (add-recipe my-mod id)))
           )]))

