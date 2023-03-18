type t = {
  name : string;
  board : Board.t;
  current : int;
}

let new_player s = { name = s; board = Board.init; current = 0 }
let roll_die = Random.int 12
let current_location p = p.current

let move_to p =
  let nboard = Board.move_to p.board roll_die in
  { name = p.name; board = nboard; current = Board.position nboard }
