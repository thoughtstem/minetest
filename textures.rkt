#lang racket

(provide noise)
(provide tri-fract)

(require 2htdp/image)


(define (tri-fract i)
  (above i
         (beside i i)))



(define (noise img (amount 100))
  (color-list->bitmap
   (map (curry noisify-color amount) (image->color-list img))
   (image-width img)
   (image-height img)))

(define (noisify-color amount color)
  (make-color
   (noisify-number amount (color-red color))
   (noisify-number amount (color-green color))
   (noisify-number amount (color-blue color))
   (color-alpha color)))

(define (noisify-number amount n)
  (min
   255
   (max
    0
    (- (+ n (random amount))
       (/ amount 2)))))

