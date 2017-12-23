#lang racket

(provide block-struct)
(provide block-struct?)
(provide compile-blocks)
(provide define-block)

(provide default-block)

(require (for-syntax racket/syntax))

(require 2htdp/image)
(require "core.rkt")


;minetest.register_node("mymod:diamond", {
;    description = "Alien Diamond",
;    tiles = {"mymod_diamond.png"},
;    is_ground_content = true,
;    groups = {cracky=3, stone=1}
;})

(struct block-struct asset-struct (tiles groups) #:transparent)

;;Makes a stub -- e.g. for default:stone
(define (default-block id)
  (block-struct (++ "" id)
                (++ "The default " id)
                '()
                '()))
 
(define-syntax (define-default-block stx)
  (syntax-case stx ()
    [(_ x )
     (with-syntax* ([name (symbol->string (format-symbol "default:~a" #'x))])
       #`(begin
           (define x (default-block name) ) 
           (provide x) 
           ) ) ]))

(define-syntax (define-default-blocks stx)
  (syntax-case stx ()
    [(_ x ... )
       #`(begin
           (define-default-block x ) ...
            
           )  ]))

(define-default-blocks
  cobble
  stonebrick
  stone_block
  mossycobble
  desert_stone
  desert_cobble
  desert_stonebrick
  desert_stone_block
  sandstone
  sandstonebrick
  sandstone_block
  desert_sandstone
  desert_sandstone_brick
  desert_sandstone_block
  silver_sandstone
  silver_sandstone_brick
  silver_sandstone_block
  obsidian
  obsidianbrick
  obsidian_block)


(define-syntax (define-block stx)
  (syntax-case stx ()
    [(_ id desc tiles ... )
     (with-syntax* ([item-id (format-id stx "~a" #'id)]
                    
                    [name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
         ;  (define my-mod (empty-mod))
           (define id (block-struct name desc (list tiles ...) '()))
           (set-my-mod! (add-block my-mod id)))
           )]))

(define/contract (export-tiles-to-file m b)
  (-> mod-struct? block-struct? (listof boolean?))
  (map save-image
       (block-struct-tiles b)
       (map (lambda (n)
              (++ (path-for m)
                  "/textures/"
                  n))
            (block-file-names b))))
 

(define/contract (block-file-names b)
  (-> block-struct? (listof string?))
  (map (lambda (num) (++ (asset-struct-name b) "_tile_" (number->string num) ".png"))
       (range (length (block-struct-tiles b)))))

(define/contract (compile-block-tiles m b)
  (-> mod-struct? block-struct? string?)
  (let ([file-names (block-file-names b)])
  (format "tiles = ~a" (compile-arr file-names STR_TYPE))))


(define/contract (compile-block-group m b)
  (-> mod-struct? block-struct? string?)
  (format "groups = ~a" (compile-ass-arr
                        (block-struct-groups b)
                        SYM_TYPE
                        INT_TYPE)))



(define/contract (compile-block m b)
  (-> mod-struct? block-struct? string?)
  (++ "-- My block is named " (asset-name m b) "\n"
      (format
"      minetest.register_node(\"~a\", {
         ~a,
         ~a,
         ~a,
       })\n\n" (asset-name m b)
           (compile-asset-description m b)
           (compile-block-tiles m b)
           (compile-block-group m b)
           )))

(define/contract (export-block-code m b)
  (-> mod-struct? block-struct? boolean?)
     (with-output-to-file (lua-file-for m) #:exists 'append 
       (lambda () (printf (++
                           (compile-block m b)
                           "\n"))))
     #t)


(define/contract (compile-blocks m is)
  (-> mod-struct? (listof block-struct?) boolean?)
  (and
   (all-true (flatten (map (curry export-tiles-to-file m) is)))
   (all-true (map (curry export-block-code m) is))
   ))

