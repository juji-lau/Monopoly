open Position

type t = {
  name : string;
  (* board : Board.t; *)
  current : Position.square;
  properties : string list;
}

let new_player (name : string) : t =
  { name; current = Position.get_initial Position.new_board; properties = [] }

(* board = Board.init; *)
(* let get_board p = p.board *)
let current_location p = p.current
let get_owned_properties (p : t) : string list = p.properties;;

Random.self_init ()

let get_name (player : t) : string = player.name

let move (x : int) (b : Position.t) (p : t) : t =
  if Position.get_index p.current + x >= 40 then
    {
      name = p.name;
      (* board = Board.move_to p.board x; *)
      current = Position.square_index b (Position.get_index p.current + x - 36);
      properties = p.properties;
    }
  else
    {
      name = p.name;
      (* board = Board.move_to p.board x; *)
      current = Position.square_index b (Position.get_index p.current + x);
      properties = p.properties;
    }

let tile_owned (pl : t) (pr : string) : bool = List.mem pr pl.properties

let buy_property (pr : string) (pl : t) : t =
  let n_prop =
    if tile_owned pl pr then pl.properties else pr :: pl.properties
  in
  {
    name = pl.name;
    (* board = pl.board; *)
    current = pl.current;
    properties = n_prop;
  }
