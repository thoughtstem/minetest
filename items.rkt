#lang racket

(provide item-struct)
(provide item-struct?)
(provide compile-items)
(provide define-item)

;ONLY provide this to the end user?
(provide custom-item)

(require 2htdp/image)
(require "core.rkt")
(require (for-syntax racket/syntax))

(struct item-struct asset-struct (image) #:transparent)

;;Makes a stub -- e.g. for default:stone
(define (default-item id)
  (item-struct (++ "" id)
                (++ "The default " id)
                (circle 0 "solid" "transparent")))

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



(define/contract (export-image-to-file m i)
  (-> mod-struct? item-struct? boolean?)
  (save-image (item-struct-image i)
              (string-append (path-for m)
                             "/textures/"
                             (asset-short-name m i) ".png")))


(define/contract (compile-item-inventory_image m i)
  (-> mod-struct? item-struct? string?)
  (format "inventory_image = ~s" (++ (asset-short-name m i) ".png") ))

;minetest.register_craftitem("test:diamond_fragments", {
;    description = "Alien Diamond Fragments",
;    inventory_image = "my_diamonds.png"
;})
(define/contract (compile-item m i)
  (-> mod-struct? item-struct? string?)
  (++ "-- My item is named " (asset-name m i) "\n"
      (format
"      minetest.register_craftitem(\"~a\", {
         ~a,
         ~a,
       })\n\n" (asset-name m i)
           (compile-asset-description m i)
           (compile-item-inventory_image m i))))

(define/contract (export-item-code m i)
  (-> mod-struct? item-struct? boolean?)
     (with-output-to-file (lua-file-for m) #:exists 'append 
       (lambda () (printf (++
                           (compile-item m i)
                           "\n"))))
     #t)




(define (custom-item name
                     (image "missing.png")
                     (desc "Missing Description"))
  (item-struct name desc image))


(define-syntax (define-item stx)
  (syntax-case stx ()
    [(_ id desc image)
     (with-syntax* ([item-id (format-id stx "~a" #'id)]
                    [name (symbol->string (format-symbol "~a" #'id))])
       #`(begin
           (define id (custom-item name image desc))
           (set-my-mod! (add-item my-mod id)))
           )]))


(define/contract (compile-items m is)
  (-> mod-struct? (listof item-struct?) boolean?)
  (and
   (all-true (map (curry export-image-to-file m) is))
   (all-true (map (curry export-item-code m) is))
   ))


