33
((3) 0 () 0 () () (h ! (equal)))
syntax
(define-block id description image)
 
  id : identifier?
  description : string?
  image : image?
syntax
(define-item id description image)
 
  id : identifier?
  description : string?
  image : image?
syntax
(define-entity id description image)
 
  id : identifier?
  description : string?
  images : (listof image?)
procedure
(schematic mappings image ...) -> schematic?
  mappings : list?
  image : image?
