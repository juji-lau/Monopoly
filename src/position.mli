(** Representation of squares on the game board.

    This module represents the different types of squares on a Monopoly game
    board including: Properties, Railroads, Utilities, Rent, Jail, Chance, and
    Free Parking *)

type square
(** The abstract type of values representing a square on the board*)

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

val new_board : t
(** The representation of a new Monopoly board with unplayed squares*)
