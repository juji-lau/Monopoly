open Position

type t = {
  name : string;
  (* board : Board.t; *)
  account : int;
  current : Position.square;
  properties : Position.square list;
  color : Raylib.Color.t;
  jail : bool;
}

let new_player (name : string) (color : Raylib.Color.t) : t =
  {
    name;
    account = 1500;
    current = Position.get_initial Position.new_board;
    properties = [];
    color;
    jail = false;
  }

let current_location p = p.current
let get_owned_properties (p : t) : Position.square list = p.properties;;

Random.self_init ()

let get_jail (p : t) : bool = p.jail
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
      jail = p.jail;
    }
  else
    {
      (* move backwards *)
      name = p.name;
      account = p.account;
      current = Position.square_index b (40 + new_position);
      properties = p.properties;
      color = p.color;
      jail = p.jail;
    }

let tile_owned (pl : t) (pr : Position.square) : bool =
  List.mem pr pl.properties

exception ExpensiveProperty

let buy_property (pr : Position.square) (pl : t) : t =
  if pl.account - Position.get_cost pr >= 0 then
    let n_prop =
      if tile_owned pl pr then pl.properties else pr :: pl.properties
    in
    {
      name = pl.name;
      account = pl.account - Position.get_cost pr;
      current = pl.current;
      properties = n_prop;
      color = pl.color;
      jail = pl.jail;
    }
  else raise ExpensiveProperty

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
        | Utility data -> 1 + helper r
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
    jail = pl.jail;
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
      jail = pl.jail;
    }
  else raise Broke

let go_to_jail (p : t) = { p with jail = true }
let free_from_jail (p : t) = { p with jail = false }
