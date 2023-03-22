type t = {
  name : string;
  board : Board.t;
  current : int;
}

let new_player s = { name = s; board = Board.init; current = 0 }
let get_board p = p.board
let current_location p = p.current;;

Random.self_init ()

let move p x =
  if x >= 36 then
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x - 36;
    }
  else
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x;
    }
