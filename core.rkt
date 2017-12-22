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



(provide mod-struct)
(provide mod-struct?)
(provide mod-struct-name)


(define STR_TYPE "~s")
(define INT_TYPE "~a")
(define SYM_TYPE "~a")

;UTIL

(define ++ string-append)

(define/contract (all-true l)
  (-> list? boolean?)
  (= (count identity l) (length l)))

;CONFIG

(define MINETEST_PATH "/home/thoughtstem/.minetest/")


;DATA STRUCTURES

(struct mod-struct (name items blocks recipes) #:transparent)


(struct asset-struct (name description) #:transparent)

(define (variableify s)
  (string-downcase
   (string-replace
    s
    " "
    "_")))

(define (asset-name a)
  (variableify (asset-struct-name a)))

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


