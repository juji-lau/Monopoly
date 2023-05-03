open Position

type t = {
  name : string;
  (* board : Board.t; *)
  account : int;
  current : Position.square;
  properties : Position.square list;
  color : Raylib.Color.t;
}

let new_player (name : string) (color : Raylib.Color.t) : t =
  {
    name;
    account = 1500;
    current = Position.get_initial Position.new_board;
    properties = [];
    color;
  }

let current_location p = p.current
let get_owned_properties (p : t) : Position.square list = p.properties;;

Random.self_init ()

let get_name (player : t) : string = player.name
let account (p : t) : int = p.account
let get_color (player : t) : Raylib.Color.t = player.color

let move (x : int) (b : Position.t) (p : t) : t =
  let new_position = Position.get_index p.current + x in
  if new_position >= 0 then
    {
      (* move fowards *)
      name = p.name;
      account = p.account;
      current = Position.square_index b (new_position mod 40);
      properties = p.properties;
      color = p.color;
    }
  else
    {
      (* move backwards *)
      name = p.name;
      account = p.account;
      current = Position.square_index b (40 + new_position);
      properties = p.properties;
      color = p.color;
    }

let tile_owned (pl : t) (pr : Position.square) : bool =
  List.mem pr pl.properties

let buy_property (pr : Position.square) (pl : t) : t =
  let n_prop =
    if tile_owned pl pr then pl.properties else pr :: pl.properties
  in
  {
    name = pl.name;
    account = pl.account;
    current = pl.current;
    properties = n_prop;
    color = pl.color;
  }

let rails_owned (b : Position.t) (pl : t) : int =
  let p_lst = pl.properties in
  let rec helper (lst : square list) : int =
    match lst with
    | [] -> 0
    | h :: r -> (
        match h with
        | Railroad data -> 1 + helper r
        | _ -> helper r)
  in
  helper p_lst

let util_owned (b : Position.t) (pl : t) : int =
  let p_lst = pl.properties in
  let rec helper (lst : square list) : int =
    match lst with
    | [] -> 0
    | h :: r -> (
        match h with
        | Railroad data -> 1 + helper r
        | _ -> helper r)
  in
  helper p_lst

let deposit (i : int) (pl : t) : t =
  {
    name = pl.name;
    account = pl.account + i;
    current = pl.current;
    properties = pl.properties;
    color = pl.color;
  }

exception Broke

let withdraw (i : int) (pl : t) : t =
  if pl.account - i >= 0 then
    {
      name = pl.name;
      account = pl.account - i;
      current = pl.current;
      properties = pl.properties;
      color = pl.color;
    }
  else raise Broke
