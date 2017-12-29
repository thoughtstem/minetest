#lang scribble/manual

@(require (for-label 2htdp/image racket "blocks.rkt"))
 
@title{The #minetest Language}
 
@defmodule[minetest/blocks]

@defform[(define-block id description image)
          #:contracts ([id identifier?]
                       [description string?]
                       [image image?])]{
 
 Adds a new block to the current mod. 
 
}
