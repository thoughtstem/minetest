#lang racket

(require 2htdp/image)

(provide compile-mod)

(require "core.rkt")


;Can be redone to return a special-compile.
  ; Then the compiler just compiles all the assets wrapped in special compiles...
  ; Then writes to the file.
(define (compile-item i)
  (special-compile
   (thunk
  
    (++ "-- My item is named " (asset-name i) "\n"
        (format
         "minetest.register_craftitem(\"~a\", ~a)\n\n"
         (asset-name i)
         (compile-v (asset->hash i)))))))

(define (compile-block b)
  (special-compile
   (thunk
    (++ "-- My block is named " (asset-name b) "\n"
        (format
         "minetest.register_node(\"~a\", ~a)\n\n"
         (asset-name b)
         (compile-v (asset->hash b)))))))

(define (compile-recipe b)
  (special-compile
   (thunk
    (++ "-- My recipe... \n"
        (format
         "minetest.register_craft(~a)\n\n"
         (compile-v (asset->hash b)))))))


(define (compile-lua-def b)
  (special-compile
   (thunk
    (++ "-- Some lua code sent by Racket \n"
        (asset-struct-more b)))))


(define (append-to-file f-name s)
    (begin
      ;(displayln (++ "Appending to file? " f-name))
      (with-output-to-file f-name #:exists 'append
        (thunk
          (printf
           (++
                s
                "--\n\n"))))
      s))

(define/contract (compile-mod m)
  (-> mod-struct? boolean?)
  (make-directory* (path-for m))
  (make-directory* (++ (path-for m) "/textures"))
  (with-output-to-file (lua-file-for m) #:exists 'replace 
       (lambda () (printf (++
                           "-- This is my mod!  It's called "
                           (mod-struct-name m)
                           "\n\n\n"))))
  (map (curry append-to-file (lua-file-for m))
       (map compile-v
            (append
             (map compile-lua-def
                  (mod-struct-lua-defs m))
             (map compile-item
                  (mod-struct-items m))
             (map compile-block
                  (mod-struct-blocks m))
             (map compile-recipe
                  (mod-struct-recipes m))
             )))
  #t)

  
 ; (and
 ;  (compile-lua-defs  m (mod-struct-lua-defs  m))
 ;  (compile-blocks m (mod-struct-blocks m))
 ;  (compile-items  m (mod-struct-items  m))
 ;  (compile-recipes  m (mod-struct-recipes  m))
 ;  )
;)


