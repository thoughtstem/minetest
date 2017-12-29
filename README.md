# The Minetest #lang for Racket

A compile-time framework for creating Minetest mods. (By compile-time, I meant that this runs before you run Minetest.  It generates Lua code that executes during the Minetest runtime.)

It gives you a powerful declarative language for creating Minetest assets.  Quick demo:

![Alt text](/examples/quick-demo.png?raw=true "Quick Demo")

This gives you:

![Alt text](/examples/quick-demo-result.png?raw=true "Quick Demo Result")

The goal is to make it as quick and easy as possible to get your art into Minetest.  (Currently only works on Linux.)

## Getting Started

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


## Authors

* **Stephen R. Foster** - *Initial work* - [srfoster](https://github.com/srfoster)


