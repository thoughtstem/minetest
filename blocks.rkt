#lang racket


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

;(struct block-struct asset-struct (tiles groups) #:transparent)

(define (block-struct name desc tiles m)
  (asset-struct name desc
                (make-immutable-hash
                         (list
                          (cons 'tiles
                                (map (curry compileable-tile m name)
                                     tiles
                                     (range (length tiles))
                                     ))))
                m))

(define (compileable-tile m prefix img i)
  (compileable-image m (format "~a_~a" prefix i) img))

;;Makes a stub -- e.g. for default:stone
(define (default-block id)
  (block-struct (++ "" id)
                (++ "The default " id)
                '()
                default-mod))
 
(define-syntax (define-default-block stx)
  (syntax-case stx ()
    [(_ x )
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'x))])
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
           (define id (block-struct name desc (list_ tiles ...) my-mod))
           (set-my-mod! (add-block my-mod id)))
           )]))

