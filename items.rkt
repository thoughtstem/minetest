#lang racket

(provide item-struct)
(provide item-struct?)
(provide compile-items)

;ONLY provide this to the end user?
(provide custom-item)

(require 2htdp/image)
(require "core.rkt")

(struct item-struct asset-struct (image) #:transparent)

(define/contract (export-image-to-file m i)
  (-> mod-struct? item-struct? boolean?)
  (save-image (item-struct-image i)
              (string-append (path-for m)
                             "/textures/"
                             (asset-name i) ".png")))


(define/contract (compile-item-inventory_image m i)
  (-> mod-struct? item-struct? string?)
  (format "inventory_image = ~s" (++ (asset-name i) ".png") ))

;minetest.register_craftitem("test:diamond_fragments", {
;    description = "Alien Diamond Fragments",
;    inventory_image = "my_diamonds.png"
;})
(define/contract (compile-item m i)
  (-> mod-struct? item-struct? string?)
  (++ "-- My item is named " (asset-name i) "\n"
      (format
"      minetest.register_craftitem(\"~a:~a\", {
         ~a,
         ~a,
       })\n\n" (mod-struct-name m) (asset-name i)
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
                     #:description (desc "Missing Description")
                     #:image (image "missing.png"))
  (item-struct name desc image))



(define/contract (compile-items m is)
  (-> mod-struct? (listof item-struct?) boolean?)
  (and
   (all-true (map (curry export-image-to-file m) is))
   (all-true (map (curry export-item-code m) is))
   ))


