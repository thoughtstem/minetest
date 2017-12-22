#lang racket

(require 2htdp/image)

(provide mod)
(provide compile-mod)

(require "core.rkt")
(require "items.rkt")
(require "blocks.rkt")
(require "recipes.rkt")

(define/contract (compile-mod m)
  (-> mod-struct? boolean?)
  (displayln m)
  (make-directory* (path-for m))
  (make-directory* (++ (path-for m) "/textures"))
  (with-output-to-file (lua-file-for m) #:exists 'replace 
       (lambda () (printf (++
                           "-- This is my mod!  It's called "
                           (mod-struct-name m)
                           "\n\n\n"))))
  (and
   (compile-blocks m (mod-struct-blocks m))
   (compile-items  m (mod-struct-items  m))
   (compile-recipes  m (mod-struct-recipes  m))
   ))


(define (mod name . things )
  (mod-struct name (filter item-struct? things)
                   (filter block-struct? things)
                   '()))
