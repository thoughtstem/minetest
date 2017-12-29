# #lang minetest for Modding Minetest in Racket

This language gives helps you create Minetest mods quickly.  Quick demo:

![Alt text](/examples/quick-demo.png?raw=true "Quick Demo")

This gives you a red block that toggles to a green block when you punch it (and vice versa):

![Alt text](/examples/quick-demo-result.png?raw=true "Quick Demo Result")

The goal of ``#lang minetest`` is to make help you get procedurally generated art on to Minetest blocks, items, particles, entities, recipes, and schematics.   We developed it for educational purposes at [ThoughtSTEM](http://thoughtstem.com).

## Getting Started

(Note: ``#lang minetest`` currently only works on Linux.)

Install Minetest:

```
sudo apt-get install minetest
```

Install Racket:

```
sudo apt-get install racket
```

Open DrRacket.  Add the `minetest` package (File > Install Package...)

Write a hello world mod...:

```
#lang minetest

(define-block hello "Hello" (text "Hello" 24 "red"))

(compile-mod my-mod)

```

Run it.  Launch Minetest.  Create a world and add the mod named ``my_racket_mod``.

You will find a hello block in your inventory:

![Alt text](/examples/hello-demo.png?raw=true "Hello")

## Current Status

This language gives you ways of converting images into:

* Blocks
* Items
* (Cube shaped) Entities 
* Recipes
* Particles
* Schematics

There are also a (limited) set of game rules for defining callbacks.  Only a few callbacks are currently supported:

* on_punch for blocks
* on_drop for blocks
* on_drop for items

## Further Reading

* [How to Design Programs: 2nd Edition](http://www.ccs.neu.edu/home/matthias/HtDP2e/) - For an amazing introduction to Racket and the equally amazing image library (2htdp/image).  Anything you can create with the 2htdp/image library can be placed into Minetest with ``#lang minetest``.  You should learn this first.

* [The docs for #lang minetest](http://docs.racket-lang.org/minetest/index.html) - I'm still working on these.  If something isn't documented, a good supplement would be the ``/examples`` directory.

* [The docs for Minetest modding in Lua](https://rubenwardy.com/minetest_modding_book/en/index.html) - For when you want to do something that ``#lang minetest`` doesn't yet support.  The compilation process tries to produce readable Lua code.  Feel free to edit it after compilation (just remember that it will get overwritten when you compile again -- so you might want to copy the compiled output directory to a new directory).

## Authors

* **Stephen R. Foster** - *Initial work* - [srfoster](https://github.com/srfoster)


