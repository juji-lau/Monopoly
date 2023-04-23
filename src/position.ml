open Yojson.Basic.Util

type square =
  | Start of {
      index : int;
      name : string;
    }
  | Property of {
      index : int;
      name : string;
      set : string;
      cost : int;
      rent : int;
      rent_set : int;
      r_1house : int;
      r_2house : int;
      r_3house : int;
      r_4house : int;
      r_hotel : int;
      building_cost : int;
      mortgage : int;
      unmortgage : int;
    }
  | Railroad of {
      index : int;
      name : string;
    }
  | Utility of {
      index : int;
      name : string;
    }
  | Rent of {
      index : int;
      name : string;
      rent : int;
    }
  | Jail of {
      index : int;
      name : string;
      cost : int;
    }
  | Go_To_Jail of {
      index : int;
      name : string;
    }
  | Chance of {
      index : int;
      name : string;
    }
  | Community_Chest of {
      index : int;
      name : string;
    }
  | Free_Parking of {
      index : int;
      name : string;
    }
  | Tax of {
      index : int;
      name : string;
      cost : int;
    }

type t = {
  board : square list;
  initial : square;
  current_int : int;
}

(*****************************************************************************)
(*Helper Functions for Processing the json file*)
(*****************************************************************************)
let start_of_j j : square =
  Start
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let property_of_j j =
  Property
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
      set = j |> member "set" |> to_string;
      cost = j |> member "cost" |> to_int;
      rent = j |> member "rent" |> to_int;
      rent_set = j |> member "rent_set" |> to_int;
      r_1house = j |> member "r_1house" |> to_int;
      r_2house = j |> member "r_2house" |> to_int;
      r_3house = j |> member "r_3house" |> to_int;
      r_4house = j |> member "r_4house" |> to_int;
      r_hotel = j |> member "r_hotel" |> to_int;
      building_cost = j |> member "building_cost" |> to_int;
      mortgage = j |> member "mortgage" |> to_int;
      unmortgage = j |> member "unmortgage" |> to_int;
    }

let railroad_of_j j : square =
  Railroad
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let utility_of_j j : square =
  Utility
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let rent_of_j j : square =
  Rent
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
      rent = j |> member "rent" |> to_int;
    }

let jail_of_j j : square =
  Jail
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
      cost = j |> member "cost" |> to_int;
    }

let go_to_jail_of_j j : square =
  Go_To_Jail
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let chance_of_j j : square =
  Chance
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let community_chest_of_j j : square =
  Community_Chest
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let free_parking_of_j j : square =
  Free_Parking
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
    }

let tax_of_j j : square =
  Tax
    {
      index = j |> member "index" |> to_int;
      name = j |> member "name" |> to_string;
      cost = j |> member "cost" |> to_int;
    }

(******************************************************************************)
(*End Helper Functions for Processing the json file*)
(******************************************************************************)

let j = Yojson.Basic.from_file "data/squares.json"

let sqr_list : square list =
  List.flatten
    ((j |> to_assoc |> List.assoc "start" |> to_list |> List.map start_of_j)
    :: (j |> to_assoc |> List.assoc "property" |> to_list
      |> List.map property_of_j)
    :: (j |> to_assoc
       |> List.assoc "community_chests"
       |> to_list
       |> List.map community_chest_of_j)
    :: (j |> to_assoc |> List.assoc "tax" |> to_list |> List.map tax_of_j)
    :: (j |> to_assoc |> List.assoc "railroads" |> to_list
      |> List.map railroad_of_j)
    :: (j |> to_assoc |> List.assoc "chance" |> to_list |> List.map chance_of_j)
    :: (j |> to_assoc |> List.assoc "jail" |> to_list |> List.map jail_of_j)
    :: (j |> to_assoc |> List.assoc "utility" |> to_list
      |> List.map utility_of_j)
    :: (j |> to_assoc |> List.assoc "free_parking" |> to_list
      |> List.map free_parking_of_j)
    :: [
         j |> to_assoc |> List.assoc "go_to_jail" |> to_list
         |> List.map go_to_jail_of_j;
       ])

let new_board =
  match sqr_list with
  | [] -> raise (Failure "Unimplemented Board")
  | h :: _ -> { board = sqr_list; initial = h; current_int = 0 }

let clear_board j = new_board

exception UnknownBoard of string

(******************************************************************************)
(*Functions that process a Position.square*)
(******************************************************************************)
let get_name (s : square) : string =
  match s with
  | Property p -> p.name
  | Railroad r -> r.name
  | Utility u -> u.name
  | Rent r -> r.name
  | Jail j -> j.name
  | Go_To_Jail g -> g.name
  | Chance c -> c.name
  | Community_Chest c -> c.name
  | Free_Parking f -> f.name
  | Tax t -> t.name
  | Start s -> s.name

let get_index (s : square) : int =
  match s with
  | Property p -> p.index
  | Railroad r -> r.index
  | Utility u -> u.index
  | Rent r -> r.index
  | Jail j -> j.index
  | Go_To_Jail g -> g.index
  | Chance c -> c.index
  | Community_Chest c -> c.index
  | Free_Parking f -> f.index
  | Tax t -> t.index
  | Start s -> s.index

exception NoSquareOfIndex

let rec square_index (b : t) (i : int) : square =
  let lst = b.board in
  let rec helper (lst2 : square list) (i : int) : square =
    match lst2 with
    | [] -> raise NoSquareOfIndex
    | h :: r -> if get_index h = i then h else helper r i
  in
  helper lst i

let rec square_name (b : t) (s : string) : square =
  let lst = b.board in
  let rec helper (lst2 : square list) (s : string) : square =
    match lst2 with
    | [] -> raise NoSquareOfIndex
    | h :: r -> if get_name h = s then h else helper r s
  in
  helper lst s
(******************************************************************************)
(*End functions that process a Position.square*)
(******************************************************************************)

let get_initial (b : t) : square = b.initial
