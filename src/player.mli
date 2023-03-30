(** Representation of player data.

    This module represents the data stored for each player in the game including
    their bank accounts and current location. It handles the rolling of the die
    as well as buying and slling properties. *)

type t
(** The abstract type of values representing players. *)

val new_player : string -> t
(** [new_player s] is the player that [s] represents. *)

val get_board : t -> Board.t
(** [get_board p] is the board that [p] represents. *)

val get_owned_properties : t -> string list
(** [get_owned_properties p] is the list of owned properties of player [p]
    categorized by the string titles of the tiles*)

val current_location : t -> int
(** [current_location p] is the current board position of the player [p]. *)

val move : int -> t -> t
(** [move x p] is the new position of the player [p] after rolling the die. *)

val tile_owned : t -> string -> bool
(** [tile_owned pl pr] returns true if the property [pr] is owned by player [pl]*)

val buy_property : t -> string -> t
(** [buy_property pl pr] adds the property string title [pr] to the purchased
    properties of player [pl]*)

(** questions about buy_property: what happens when: a misspelled or nonexistant
    property is bought, if a property is bought twice by the same player, when
    the same property is bought by a different player, how is the string list of
    properties sortged?*)
