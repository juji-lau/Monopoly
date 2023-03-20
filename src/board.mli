(** Representation of game board data.

    This module represents the data stored in each board spot. It handles moving
    positions on the board. *)

type t
(** The abstract type of values representing a board position. *)

val init : t
(** [init p] initializes a new board for player [p]. *)

val position : t -> int
(** [position p] is the current location of player [p]. *)

val move_to : t -> int -> t
(** [move_to b i] is the new board value after moving by [i] spots. *)
