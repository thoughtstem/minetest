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
                          (cons 'groups (make-hash
                                           (list
                                             (cons "choppy" 1))))
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
  tree
  wood
  leaves
  sapling
  apple
  jungletree
  junglewood
  jungleleaves
  junglesapling
  pine_tree
  pine_wood
  pine_needles
  pine_sapling
  acacia_tree
  acacia_wood
  acacia_leaves
  acacia_sapling
  aspen_tree
  aspen_wood
  aspen_leaves
  aspen_sapling
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

(define wool-mod
  (mod-struct "wool" '() '() '() '() '()))

(define-syntax (define-wool-block stx)
  (syntax-case stx ()
    [(_ x )
     (with-syntax* ([name (symbol->string (format-symbol "~a" #'x))])
       #`(begin
           (define x (wool-block name) ) 
           (provide x) 
           ) ) ]))

(define-syntax (define-wool-blocks stx)
  (syntax-case stx ()
    [(_ x ... )
       #`(begin
           (define-wool-block x ) ...
           )  ]))

(define (wool-block id)
  (block-struct (++ "" id)
                (++ "Wool: " id)
                '()
                wool-mod))

(define-wool-blocks
  white
  grey
  black
  red
  yellow
  green
  cyan
  blue
  magenta
  orange
  violet
  brown
  pink
  dark_grey
  dark_green)

