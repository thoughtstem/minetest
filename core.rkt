#lang racket

(provide ++)
(provide zip)
(provide all-true)
(provide asset-struct)
(provide mod-struct)
(provide mod-struct-blocks)
(provide mod-struct-items)
(provide mod-struct-recipes)
(provide mod-struct-lua-defs)
(provide mod-struct-entities)
(provide MINETEST_PATH)
(provide path-for)
(provide asset-name)
(provide asset-short-name)

(provide add-behaviour)

(provide asset-struct)
(provide asset-struct?)
(provide asset-struct-name)
(provide asset-struct-more)
(provide asset->hash)

(provide compile-v)
(provide special-compile)
(provide special-compile-f)

(provide compile-asset-description)

(provide anonymous-compileable-image)

(provide lua-file-for)

(provide path-for)

(provide tree-map)



(provide mod-struct)
(provide mod-struct?)
(provide mod-struct-name)

(provide add-to-more)

(provide set-my-mod!)
(provide my-mod)

(provide add-item)
(provide add-block)
(provide add-recipe)
(provide add-lua-def)
(provide add-entity)


(provide list_)

(provide variableify)

(require (for-syntax racket/syntax))

;UTIL

(define (list_ x)
  (if (list? x)
      x
      (list x)))

(define ++ string-append)

(define/contract (all-true l)
  (-> list? boolean?)
  (= (count identity l) (length l)))

(define (tree-map f tree)
  (if (list? tree)
      (map (curry tree-map f) tree)
      (f tree)
      ))

(define (zip l1 l2)
  (map list l1 l2))

(define (with-index l1)
  (zip l1 (range (length l1))))

(define (filter-index pred l)
  (map first
       (filter (lambda (x) (pred (second x)))
               (with-index l))))

(define (evens l)
  (filter-index even? l))

(define (odds l)
  (filter-index odd? l))

(provide in-pairs)
(define (in-pairs l)
  (zip (evens l) (odds l)))

;CONFIG

;(define MINETEST_PATH "/home/thoughtstem/.minetest/")

(define home (find-system-path 'home-dir))

(define MINETEST_PATH
  (cond
    [(eq? (system-type 'os) 'unix) (string-append (path->string home) ".minetest/")]
    [(eq? (system-type 'os) 'macosx) (string-append (path->string home) "Library/Application Support/minetest")]
    [(eq? (system-type 'os) 'windows) "C:/minetest/"]))

;DATA STRUCTURES

(struct special-compile (f))

(struct mod-struct (name items blocks recipes entities lua-defs) )

(struct asset-struct (name description more mod) #:transparent)

(define (asset->hash a)
  (hash-set
   (asset-struct-more a)
   'description
   (asset-struct-description a)))


(define (add-behaviour target kv m)
  (let ([updated-target (add-to-more target kv)])
    (set! my-mod
          (replace-in-mod m target updated-target))))
                

(define (replace-in-mod m t1 t2)
  (mod-struct
     (mod-struct-name  m)
     (replace-in-list (mod-struct-items m) t1 t2)
     (replace-in-list (mod-struct-blocks m) t1 t2)
     (replace-in-list (mod-struct-recipes m) t1 t2)
     (replace-in-list (mod-struct-entities m) t1 t2)
     (replace-in-list (mod-struct-lua-defs m) t1 t2)))

(define (replace-in-list l t1 t2)
  (map (lambda (x)
         (if (eq? (asset-struct-name x) (asset-struct-name t1))
             t2
             x)) l))

(define (add-to-more a kv)
  (let ([new-more   (hash-set
                     (asset-struct-more a)
                     (string->symbol (variableify (first kv)))
                     (second kv))])
    (struct-copy asset-struct a
                 [more new-more])))



(provide default-mod)
(define default-mod
  (mod-struct "default" '() '() '() '() '()))

(define my-mod
  (mod-struct "my_racket_mod" '() '() '() '() '()))

(define (set-my-mod! m)
  (set! my-mod m))

;NOTE: Could make a syntax for defining adders automatically...
(define (add-item m i)
  (struct-copy mod-struct m
               [items (cons i (mod-struct-items m))]))

(define (add-recipe m i)
  (struct-copy mod-struct m
               [recipes (cons i (mod-struct-recipes m))]))

(define (add-block m i)
  (struct-copy mod-struct m
               [blocks (cons i (mod-struct-blocks m))]))

(define (add-lua-def m i)
  (struct-copy mod-struct m
               [lua-defs (cons i (mod-struct-lua-defs m))]))

(define (add-entity m i)
  (struct-copy mod-struct m
               [entities (cons i (mod-struct-entities m))]))



(define (variableify s)
  (string-downcase
   (string-replace
    (string-replace
     s
     " "
     "_")
    "-"
    "_")))

(define (asset-short-name m a)
  (second (string-split (asset-name m a) ":")))

(define (asset-name a)
  (let ([m  (asset-struct-mod a)]
        [name (variableify (asset-struct-name a))])
    (if (string-contains? name "default:")
              name
              (++ (mod-struct-name m) ":" name))
    ))

(define (asset-description a)
  (asset-struct-description a))



(define/contract (lua-file-for m)
  (-> mod-struct? string?)
  (string-append
                    (path-for m)
                              "/init.lua"))


(define/contract (path-for m)
  (-> mod-struct? string?)
  (string-append MINETEST_PATH "/mods/" (mod-struct-name m)))

(define/contract (compile-v v)
  (-> any/c any/c)
  (cond [(asset-struct? v) (format "~s" (asset-name v))]
        [(image? v) (compile-v (anonymous-compileable-image v))]
        [(special-compile? v) ((special-compile-f v))]
        [(string? v) (format "\"~a\"" v)]
        [(number? v) (number->string v)]
        [(boolean? v) (if v "true" "false")]
        [(hash? v) (compile-hash v)]
        [(list? v) (++ "{" (string-join (map compile-v v) ",") "}")]
        [else
         (displayln v)
         (error "Non compilable value")]))

(define/contract (compile-kv k v)
  (-> (or/c string? symbol? number?) any/c string?)
    (format "~a = ~a" k (compile-v v)))

(define/contract (compile-hash h)
  (-> hash? string?)
  (let ([compiled-keys (map (lambda(k) (compile-kv k (hash-ref h k)))
                            (hash-keys h))])
    (++ "{\n"
        (string-join
         (map (curry string-append "  ")
              compiled-keys)
         ",\n")
        "\n}")))

(define/contract (compile-asset-description m i)
  (-> mod-struct? asset-struct? string?)
    (compile-kv "description" (asset-description i)))




;IMAGE SUPPORT.  Could go in a different file...

(require 2htdp/image) 

(provide compileable-image)
(define (compileable-image m id img)
  (special-compile
     (lambda ()
       (export-image-to-file m id img )
       (format "~s" (++ id ".png"))
       )))

(define (anonymous-compileable-image img)
  (compileable-image my-mod (random-file-id) img))

(provide random-file-id)
(define (random-file-id)
  (number->string (random 1000000)))

(define/contract (export-image-to-file m id img)
  (-> mod-struct? string? image? boolean?)
  (save-image img
              (string-append (path-for m)
                             "/textures/"
                             id ".png")))
;END IMAGE SUPPORT

(provide append-to-file)
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
