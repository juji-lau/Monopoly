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

type t = square list
(* { s0 : start; s1 : property; s2 : community_chest; s3 : property; s4 : tax;
   s5 : railroad; s6 : property; s7 : chance; s8 : property; s9 : property; s10
   : jail; s11 : property; s12 : utility; s13 : property; s14 : property; s15 :
   railroad; s16 : property; s17 : community_chest; s18 : property; s19 :
   property; s20 : free_parking; s21 : property; s22 : chance; s23 : property;
   s24 : property; s25 : railroad; s26 : property; s27 : property; s28 :
   utility; s29 : property; s30 : go_to_jail; s31 : property; s32 : property;
   s33 : community_chest; s34 : property; s35 : railroad; s36 : chance; s37 :
   property; s38 : tax; s39 : property; } *)

(* let new_board = [ start_sqr; mediterranean_avenue; first_Chest;
   baltic_Avenue; first_Tax; first_Rail; oriental_Avenue; first_Chance;
   vermont_Avenue; connecticut_Avenue; jail_Sqr; charles_Place; electric;
   states_Avenue; virginia_Avenue; second_Rail; james_Place; second_Chest;
   tennessee_Avenue; ny_Avenue; free_parking; kentucky_Avenue; second_Chance;
   indiana_Avenue; illinois_Avenue; third_Rail; atlantic_Avenue; ventnor_Avenue;
   water_works; marvin_Gardens; go_to_jail; pacific_Avenue; nc_Avenue;
   third_Chest; pennsylvania_Avenue; fourth_Rail; third_Chance; park_Place;
   second_Tax; boardwalk; ]

   let clear_board b = new_board *)

(* let new_board = {s0} *)
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

let get_name s =
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

let j = Yojson.Basic.from_file "data/squares.json"

let new_board =
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

let clear_board j = new_board
(* let from_json j = { start = j |> to_assoc |> List.assoc "start" |> List.map
   start_of_j (* rooms = json |> to_assoc |> List.assoc "rooms" |> to_list |>
   List.map room_of_json; starting_room = json |> to_assoc |> List.assoc "start
   room" |> to_string; *); } *)

(* to match index j -> to_assoc -> pattern match for start, property, etc. ->
   get second element of tuple -> to_list -> for each element -> to_assoc -> rec
   match for index*)
(* let rec get_name i = match lst with | [] -> failwith "NoSuchIndex" | h :: t
   -> if List.assoc "index" h = i then List.assoc "name" h else
   get_name_from_index i t *)

(* let rec get_square_type_from_index i *)
(* let rec get_name s json = match json |> to_assoc with | [] -> failwith
   "get_name error" | h :: t -> ( match h with | property_group, lst -> ( match
   to_list lst with | [] -> get_name | h :: t -> if List.assoc h "index" = )) *)
(* *)

exception UnknownBoard of string
