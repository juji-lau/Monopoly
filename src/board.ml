type t = { position : int }
type t = { current : int }

let init = { current = 0 }
let position b = b.current
let move_to b i = { current = b.current + i }

(** (type t = {
  board : Position.square list;
  current : Position.square
  }

exception UnimplemetedBoard
let init = let b = Position.new_board in 
match b with
|[] -> raise UnimplemetedBoard
|h ::_ -> let curr = h in 
{board = b ; current = curr }

let position b = b.current

let move_to b i = 
  let rec mover x i lst= 
    if x + i < 20 then
      match lst with 
      |[] -> raise UnimplemetedBoard
      |h :: t -> match h with 
      |Position.square.Property P {index = ind} -> if ind = x+i then 
        {board = b.board; current = P} else mover x i t
      |Position.square.Railroad R {index = ind} -> if ind = x+i then 
        {board = b.board; current = R} else mover x i t
      |Position.square.Utility U {index = ind} -> if ind = x+i then 
        {board = b.board; current = U} else mover x i t
      |Position.square.Rent E {index = ind} -> if ind = x+i then 
        {board = b.board; current = E} else mover x i t
    else
    {position = b.position + i - 36}
    
    match position b with 
| Position.square.Property P {index = x} -> x + i < 20 
| Position.square.Railroad R {index = x} -> x + i < 20 
  
)*)
