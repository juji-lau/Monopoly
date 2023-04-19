type t = {
  name : string;
  board : Board.t;
  current : int;
  properties : string list;
}

let new_player s =
  { name = s; board = Board.init; current = 0; properties = [] }

let get_board p = p.board
let current_location p = p.current
let get_owned_properties (p : t) : string list = p.properties;;

Random.self_init ()

let move x p =
  if current_location p + x >= 40 then
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x - 40;
      properties = p.properties;
    }
  else
    {
      name = p.name;
      board = Board.move_to p.board x;
      current = current_location p + x;
      properties = p.properties;
    }

let tile_owned (pl : t) (pr : string) : bool = List.mem pr pl.properties

let buy_property (pr : string) (pl : t) : t =
  let n_prop =
    if tile_owned pl pr then pl.properties else pr :: pl.properties
  in
  {
    name = pl.name;
    board = pl.board;
    current = pl.current;
    properties = n_prop;
  }
