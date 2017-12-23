#lang racket

(provide ++)
(provide all-true)
(provide asset-struct)
(provide mod-struct)
(provide mod-struct-blocks)
(provide mod-struct-items)
(provide mod-struct-recipes)
(provide MINETEST_PATH)
(provide path-for)
(provide asset-name)
(provide asset-short-name)

(provide asset-struct)
(provide asset-struct-name)

(provide compile-arr)
(provide compile-ass-arr)

(provide compile-asset-description)

(provide lua-file-for)

(provide STR_TYPE)
(provide SYM_TYPE)
(provide INT_TYPE)

(provide path-for)

(provide tree-map)



(provide mod-struct)
(provide mod-struct?)
(provide mod-struct-name)
(provide mod-struct-lua-defs)
(provide set-my-mod!)
(provide my-mod)

(provide add-item)
(provide add-block)
(provide add-recipe)
(provide add-lua-def)

(define STR_TYPE "~s")
(define INT_TYPE "~a")
(define SYM_TYPE "~a")
(define ARR_TYPE "ARR_TYPE")


;UTIL

(define ++ string-append)

(define/contract (all-true l)
  (-> list? boolean?)
  (= (count identity l) (length l)))

(define (tree-map f tree)
  (if (list? tree)
      (map (curry tree-map f) tree)
      (f tree)
      ))

;CONFIG

(define MINETEST_PATH "/home/thoughtstem/.minetest/")


;DATA STRUCTURES

(struct mod-struct (name items blocks recipes lua-defs) #:transparent)

(struct asset-struct (name description) #:transparent)

(define my-mod
  (mod-struct "my_racket_mod" '() '() '() '()))

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




(define (variableify s)
  (string-downcase
   (string-replace
    s
    " "
    "_")))

(define (asset-short-name m a)
  (second (string-split (asset-name m a) ":")))

(define (asset-name m a)
  (let ([name (variableify (asset-struct-name a))])
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

(define/contract (compile-asset-description m i)
  (-> mod-struct? asset-struct? string?)
    (format "description = ~s" (asset-description i)))


(define/contract (compile-arr arr type)
  (-> list? any/c string?)
  (format
   "{~a}"
   (string-join
    (map (lambda (x) (format type x))
         arr) ",")))


(define/contract (compile-ass-arr arr type1 type2)
  (-> list? any/c any/c string?)
  (format
   "{~a}"
   (string-join
    (map (lambda (x) (format (++ type1 "=" type2) (first x) (second x)))
         arr) ",")))


