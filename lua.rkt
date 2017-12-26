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



;Some default code snippets

(define-lua particles-def
"
function(file,num,vx,vy,vz,size) 
  return function(pos, node, player, pointed_thing)
    for i=1,num do
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
     end
  end
end
")

(provide particles)
(define (particles img (num 30) (vx 2) (vy 2) (vz 2) (size 5))
  (ref-lua particles-def img num vx vy vz size))



(define-lua spawn-def
"
function(entity) 
  return function(pos, node, player, pointed_thing)
    local new_pos = {
      x=pos.x,
      y=pos.y + 1,
      z=pos.z
    }
    minetest.add_entity(new_pos, entity)
  end
end
")


(provide spawn)
(define (spawn entity)
  (ref-lua spawn-def entity))


(define-lua place-schematic-def
"
function(path) 
  return function(pos, node, player, pointed_thing)
    minetest.place_schematic(pos,path,'random',nil,false)
  end
end
")


(provide place-schematic)
(define (place-schematic schem)
  (ref-lua place-schematic-def schem))






(define-lua on-punch-sequence-def
"
function(fs) 
  return function(pos, node, player, pointed_thing)
    for i,f in ipairs(fs) do f(pos,node,player,pointed_thing) end
  end
end
")

(provide on-punch-sequence)
(define (on-punch-sequence . fs)
  (ref-lua on-punch-sequence-def fs))

