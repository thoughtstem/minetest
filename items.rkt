#lang racket


(provide define-item)

(require 2htdp/image)
(require "core.rkt")
(require (for-syntax racket/syntax))

;(struct item-struct asset-struct (image) #:transparent)

;Was a struct... not anymore...
(define (item-struct id desc img m)
  (asset-struct id desc (make-immutable-hash
                         (list
                          (cons 'inventory_image
                                (compileable-image m id img))
                          ))
                m))


(define-syntax (define-item stx)
  (syntax-case stx ()
    [(_ id desc image)
     (with-syntax* ([item-id (format-id stx "~a" #'id)]
                    [name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (item-struct name desc image my-mod))
           (set-my-mod! (add-item my-mod id)))
           )]))

;id desc img m
;;Makes a stub -- e.g. for default:stone
(define (default-item id)
  (item-struct (++ "" id)
               (++ "The default " id)
               (circle 0 "solid" "transparent")
               default-mod))





(define-syntax (define-default-item stx)
  (syntax-case stx ()
    [(_ x )
     (with-syntax* ([name (symbol->string (format-symbol "default:~a" #'x))])
       #`(begin
           (define x (default-item name) ) 
           (provide x) 
           ) ) ]))

(define-syntax (define-default-items stx)
  (syntax-case stx ()
    [(_ x ... )
       #`(begin
           (define-default-item x ) ...
            
           )  ]))


(define-default-items
  stick
paper
book
book_written
skeleton_key
coal_lump
iron_lump
copper_lump
tin_lump
mese_crystal
gold_lump
diamond
clay_lump
steel_ingot
copper_ingot
tin_ingot
bronze_ingot
gold_ingot
mese_crystal_fragment
clay_brick
obsidian_shard
flint
pick_wood
pick_stone
pick_steel
pick_bronze
pick_mese
pick_diamond
shovel_wood
shovel_stone
shovel_steel
shovel_bronze
shovel_mese
shovel_diamond
axe_wood
axe_stone
axe_steel
axe_bronze
axe_mese
axe_diamond
sword_wood
sword_stone
sword_steel
sword_bronze
sword_mese
sword_diamond
key)


