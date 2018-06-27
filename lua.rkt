#lang racket

(provide define-lua-raw)
(provide define-lua)
(provide ref-lua)
(provide sequence)

(require "core.rkt")
(require (for-syntax racket/syntax))



(define (lua-def id code m)
  (asset-struct id ""
                (++ (variableify id) " = "
                    code)
                m))

(define (lua-raw id code m)
  (asset-struct id ""
                code
                m))

(define (sequence . refs)
  (special-compile
   (thunk
    (format "function()\n ~a \nend"
            (string-join
             (map (curryr ++ "()") (map compile-v refs))
             "\n"))
    )))

(define (ref-lua ref . args)
  (special-compile
   (thunk
    (let ([base (format "~a" (variableify (asset-struct-name ref)))])
      (if (empty? args)
          base
          (format "~a(~a)" base
                  (string-join
                   (map compile-v args)
                   ",")))))))


(define-syntax (define-lua stx)
  (syntax-case stx ()
    [(_ id body-string)
            (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
              #`(begin
                  (define id (lua-def name body-string my-mod) )
                  (set-my-mod! (add-lua-def my-mod id))
                  ) ) ]))

(define-syntax (define-lua-raw stx)
  (syntax-case stx ()
    [(_ id body-string)
            (with-syntax* ([name (symbol->string (format-symbol "~a" #'id))])
              #`(begin
                  (define id (lua-raw name body-string my-mod) )
                  (set-my-mod! (add-lua-def my-mod id))
                  ) ) ]))


(define-syntax (define-lua-callback stx)
  (syntax-case stx ()
    [(_ name context ) 
     (with-syntax* ([def-id (format-id stx "on-~a-~a-def" #'context #'name)]
                    [ref-id (format-id stx "on-~a-~a" #'context #'name)]
                    [wrapper-id (format-id stx "~a-wrapper" #'context)]
                    )
       #`(begin
           (define-lua def-id
             (name wrapper-id))

           (provide ref-id)
           (define (ref-id . args)
             (apply (curry ref-lua def-id) args))

          ))]))

(define-syntax (define-lua-callbacks stx)
  (syntax-case stx ()
    [(_ name context ... ) 
     #'(begin
         (define-lua-callback name context)
         ...)]))


(define (block-punch-wrapper code)
  (format
   "return function(pos, node, player, pointed_thing)
      ~a
    end"
   code))

(define (block-use-wrapper code)
  (format
   "return function(itemstack, player, pointed_thing)
      local pos = minetest.get_pointed_thing_position(pointed_thing, true) or {x=player:get_pos().x, y=player:get_pos().y+1, z=player:get_pos().z}
      ~a
    end"
   code))

(define (item-use-wrapper code)
  (format
   "return function(itemstack, player, pointed_thing)
      local pos = minetest.get_pointed_thing_position(pointed_thing, true) or {x=player:get_pos().x, y=player:get_pos().y+1, z=player:get_pos().z}
      ~a
    end"
   code))

(define (item-drop-wrapper code)
  (format
   "return function(itemstack, dropper, pos)
      ~a
      itemstack:take_item()
      return itemstack
    end"
   code))

(define (particles wrapper)
  (format
"function(num,file)
  vx = 5
  vy = 5
  vz = 5
  size = 10
  ~a
end"
  (wrapper
    "for i=1,num do
		  minetest.add_particle({
			  pos = pos,
			  velocity = {x=vx*(math.random()-.5), y=vy*(math.random()-.5), z=vz*(math.random()-.5)},
			  acceleration = {x=0, y=0, z=0},
			  expirationtime = 1,
			  size = size,
			  collisiondetection = false,
			  vertical = false,
			  texture = file,
			  playername = 'singleplayer'
		   })

   end")
))



(define-lua-callbacks
  particles
  block-punch
  block-use
  item-use
  item-drop)




(define (spawn wrapper)
  (format
"function(entity) 
  ~a
end"
  (wrapper
    "local new_pos = {
      x=pos.x,
      y=pos.y + 1,
      z=pos.z
    }
    minetest.add_entity(new_pos, entity)")
))


(define-lua-callbacks
  spawn
  block-punch
  block-use
  item-use
  item-drop)


(define (place-schematic wrapper)
  (format
"function(path) 
  ~a
end"
  (wrapper
    "minetest.place_schematic(pos,path,'random',nil,false)")
))


(define-lua-callbacks
  place-schematic
  block-punch
  block-use
  item-use
  item-drop)


(define (place-block wrapper)
  (format
"function(block) 
  ~a
end"
  (wrapper
    "minetest.set_node(pos,{name=block})")
))

(define-lua-callbacks
  place-block
  block-punch
  block-use
  item-use
  item-drop)




(define-lua on-block-punch-sequence-def
"
function(fs) 
  return function(pos, node, player, pointed_thing)
    for i,f in ipairs(fs) do f(pos,node,player,pointed_thing) end
  end
end
")

(provide on-block-punch-sequence)
(define (on-block-punch-sequence . fs)
  (ref-lua on-block-punch-sequence-def fs))

(define-lua on-block-use-sequence-def
"
function(fs) 
  return function(itemstack, player, pointed_thing)
    for i,f in ipairs(fs) do f(itemstack,player,pointed_thing) end
  end
end
")

(provide on-block-use-sequence)
(define (on-block-use-sequence . fs)
  (ref-lua on-block-use-sequence-def fs))

(define-lua on-item-use-sequence-def
"
function(fs) 
  return function(itemstack, player, pointed_thing)
    for i,f in ipairs(fs) do f(itemstack,player,pointed_thing) end
  end
end
")

(provide on-item-use-sequence)
(define (on-item-use-sequence . fs)
  (ref-lua on-item-use-sequence-def fs))

(define-lua on-item-drop-sequence-def
"
function(fs) 
  return function(itemstack, dropper, pos)
    for i,f in ipairs(fs) do f(itemstack, dropper, pos) end
    itemstack:take_item()
    return itemstack
  end
end
")

(provide on-item-drop-sequence)
(define (on-item-drop-sequence . fs)
  (ref-lua on-item-drop-sequence-def fs))







