#lang racket

(provide block-struct)
(provide block-struct?)
(provide compile-blocks)

(require 2htdp/image)
(require "core.rkt")


;minetest.register_node("mymod:diamond", {
;    description = "Alien Diamond",
;    tiles = {"mymod_diamond.png"},
;    is_ground_content = true,
;    groups = {cracky=3, stone=1}
;})

(struct block-struct asset-struct (tiles groups) #:transparent)

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
  (++ "-- My block is named " (asset-name b) "\n"
      (format
"      minetest.register_node(\"~a:~a\", {
         ~a,
         ~a,
         ~a,
       })\n\n" (mod-struct-name m) (asset-name b)
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

