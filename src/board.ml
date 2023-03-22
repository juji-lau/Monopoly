(**
type t = {
  current : int
  }


let init =  {current = 0}
let position b = b.current

let move_to b i = 
  {current = b.current +i}
  
*)

type t = {
  board : Position.square list;
  current : Position.square;
  current_int : int;
}

exception UnimplemetedBoard

let init =
  let b = Position.new_board in
  match b with
  | [] -> raise UnimplemetedBoard
  | h :: _ ->
      let curr = h in
      { board = b; current = curr; current_int = 0 }

let position b = b.current
let position_int b = b.current_int

let move_to b i =
  let x = b.current_int + i in
  let lst = b.board in
  if x > 39 then
    let curr = List.nth lst (x - 39) in
    { board = b.board; current = curr; current_int = x - 39 }
  else
    let curr = List.nth lst x in
    { board = b.board; current = curr; current_int = x }
