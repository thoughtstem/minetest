#lang racket


(provide define-entity)

(require (for-syntax racket/syntax))

(require 2htdp/image)
(require "lua.rkt")
(require "mob-api-raw.rkt")
(require "core.rkt")

;;Injects a ton of code by PilzAdam into the user's mod.  Allowing for calls to register_mob
;;  Instead of register_entity.
(define-lua-raw mob-api mob-api-raw)


;minetest.register_node("mymod:diamond", {
;    description = "Alien Diamond",
;    tiles = {"mymod_diamond.png"},
;    is_ground_content = true,
;    groups = {cracky=3, stone=1}
;})

(define (entity-struct name desc tiles m)
  (asset-struct name desc
                (make-immutable-hash
                         (list
                          (cons 'type "animal")
                          (cons 'hp_max 1)
                          (cons 'visual "cube")
                          (cons 'physical #t)
                          (cons 'walk_velocity 1)
                          (cons 'run_velocity 1)
                          (cons 'visual_size
                                (make-hash
                                 (list
                                  (cons 'x 1)
                                  (cons 'y 1))))
                          
                          ;;For this one, we also need to make sure the length = 6,
                          ;;  So we pad by duplicating the first entry...
                          (cons 'textures
                                (map anonymous-compileable-image
                                     (pad tiles 6)))))
                m))

(define (pad tiles len)
  (if (= len (length tiles))
      tiles
      (pad (cons (first tiles) tiles) len)))


(define-syntax (define-entity stx)
  (syntax-case stx ()
    [(_ id desc tiles ... )
     (with-syntax* ([item-id (format-id stx "~a" #'id)]
                    [name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (entity-struct name desc (list_ tiles ...) my-mod))
           (set-my-mod! (add-entity my-mod id)))
           )]))









#;(
;;Makes a stub -- e.g. for default:stone
(define (default-block id)
  (entity-struct (++ "" id)
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
)


