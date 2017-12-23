#lang racket

(provide recipe-struct)
(provide recipe-struct?)
(provide compile-recipes)
(provide define-recipe)

(require (for-syntax racket/syntax))

(require 2htdp/image)
(require "core.rkt")

(struct recipe-struct asset-struct (num output inputs) #:transparent)

(define (compile-recipe-output m r)
  (let ([name (asset-name m (recipe-struct-output r))])
  (format "output = \"~a ~a\""
          name
          (recipe-struct-num r))))

(define (compile-recipe-input m r)
  (let ([asset-names (map
                      (curry asset-name m)
                      (recipe-struct-inputs r))])
    (format "recipe = ~a" (compile-arr asset-names STR_TYPE))))

(define (compile-recipe-input-shaped m r)
  (let ([asset-names (tree-map
                      (curry asset-name m)
                      (recipe-struct-inputs r))])
    (format "recipe = {~a}" (string-join
                               (map (curryr compile-arr STR_TYPE) asset-names)
                               ",\n"))))

;minetest.register_craft({
;    type = "shapeless",
;    output = "mymod:diamond",
;    recipe = {"mymod:diamond_fragments", "mymod:diamond_fragments", "mymod:diamond_fragments"}
;})
(define/contract (compile-shapeless-recipe m r)
  (-> mod-struct? recipe-struct? string?)
  (++ "-- My recipe is named " (asset-name m r) "\n"
      (format
"      minetest.register_craft({
         type = \"shapeless\",
         ~a,
         ~a,
       })\n\n" 
           (compile-recipe-output m r)
           (compile-recipe-input  m r))))

(define/contract (compile-shaped-recipe m r)
  (-> mod-struct? recipe-struct? string?)
  (++ "-- My recipe is named " (asset-name m r) "\n"
      (format
"      minetest.register_craft({
         ~a,
         ~a,
       })\n\n" 
           (compile-recipe-output m r)
           (compile-recipe-input-shaped  m r))))

(define (is-shaped? r)
  (list? (first (recipe-struct-inputs r))))

(define/contract (compile-recipe m r)
  (-> mod-struct? recipe-struct? string?)
  (if (is-shaped? r)
      (compile-shaped-recipe m r)
      (compile-shapeless-recipe m r)))

;;Unshaped version...
(define-syntax (define-recipe stx)
  (syntax-case stx (make: from:)
    [(_ id make: num output from: (i1 i2 i3) (i4 i5 i6) (i7 i8 i9) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2 i3)
                                                              (list i4 i5 i6)
                                                              (list i7 i8 i9))))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: (i1 i2) (i3 i4) (i5 i6) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2)
                                                              (list i3 i4)
                                                              (list i5 i6))))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: (i1 i2 i3) (i4 i5 i6) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2 i3)
                                                              (list i4 i5 i6))))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: (i1 i2) (i3 i4) ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list (list i1 i2)
                                                              (list i3 i4))))
           (set-my-mod! (add-recipe my-mod id)))
           )]
    [(_ id make: num output from: items ... ) 
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (recipe-struct name "" num output (list items ...)))
           (set-my-mod! (add-recipe my-mod id)))
           )]))


(define/contract (export-recipe-code m i)
  (-> mod-struct? recipe-struct? boolean?)
     (with-output-to-file (lua-file-for m) #:exists 'append 
       (lambda () (printf (++
                           (compile-recipe m i)
                           "\n"))))
     #t)




(define/contract (compile-recipes m rs)
  (-> mod-struct? (listof recipe-struct?) boolean?)
   (all-true (map (curry export-recipe-code m) rs)))


