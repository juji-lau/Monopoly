(** Representation of squares on the game board.

    This module represents the different types of squares on a Monopoly game
    board including: Properties, Railroads, Utilities, Rent, Jail, Chance, and
    Free Parking *)

type square
(** The abstract type of values representing a square on the board*)

type t
(** The abstract type of values representing a board*)

exception UnknownBoard of string

val new_board : square list
(** The representation of a new Monopoly board with unplayed squares*)

val clear_board : square list -> square list
(** [clear_board b] is board [b] with unplayed squares as if clearing it become
    a new board. Raises [UnknownBoard b] if [b] is not a board currently being
    played. Example: clearing board b returns a new board.*)
