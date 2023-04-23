(** Representation of squares on the game board.

    This module represents the different types of squares on a Monopoly game
    board including: Properties, Railroads, Utilities, Rent, Jail, Chance, and
    Free Parking *)

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
    }  (** The abstract type of values representing a square on the board*)

type t
(** The abstract type of values representing a board*)

val get_name : square -> string
(**[get_name t] returns the string name of the square represented by [t]. *)

val get_index : square -> int
(**[get_index t] returns the integer index position of the square represented by
   [t] on it's monopoly board. *)

val get_initial : t -> square
(**[get_initial b] gives the initial square on board b*)

exception UnknownBoard of string

val square_index : t -> int -> square
(**[square_index b i] is the square on the board [b] at position [i]*)

val square_name : t -> string -> square
(** [square_name b s] gives the square with name s *)

val new_board : t
(** The representation of a new Monopoly board with unplayed squares*)
