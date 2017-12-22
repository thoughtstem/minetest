#lang racket

(provide recipe-struct)
(provide recipe-struct?)
(provide compile-recipes)

(require 2htdp/image)
(require "core.rkt")

(struct recipe-struct asset-struct (input output) #:transparent)


;minetest.register_craft({
;    output = "mymod:diamond_chair 99",
;    recipe = {
;        {"mymod:diamond_fragments", "", ""},
;        {"mymod:diamond_fragments", "mymod:diamond_fragments", ""},
;        {"mymod:diamond_fragments", "mymod:diamond_fragments",  ""}
;    }
;})
(define/contract (compile-recipe m i)
  (-> mod-struct? recipe-struct? string?)
  (++ "-- My recipe is named " (asset-name i) "\n"
      (format
"      minetest.register_recipe(\"~a:~a\", {
         ~a,
         ~a,
       })\n\n" (mod-struct-name m) (asset-name i)
           (compile-recipe-output m i)
           (compile-recipe-input  m i))))

(define/contract (export-recipe-code m i)
  (-> mod-struct? recipe-struct? boolean?)
     (with-output-to-file (lua-file-for m) #:exists 'append 
       (lambda () (printf (++
                           (compile-recipe m i)
                           "\n"))))
     #t)




(define/contract (compile-recipes m rs)
  (-> mod-struct? (listof recipe-struct?) boolean?)
   (all-true (map (curry export-recipe-code m) is)))


