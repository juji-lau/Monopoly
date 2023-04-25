(** Representation of player data.

    This module represents the data stored for each player in the game including
    their bank accounts and current location. It handles the rolling of the die
    as well as buying and slling properties. *)

type t
(** The abstract type of values representing players. *)

val new_player : string -> t
(** [new_player s] is the player that [s] represents. *)

(*_______Functions that give data about the player_______*)

val get_name : t -> string
(** [get_name player] is the string representation of the [player]. *)

val account : t -> int
(** [account p] is the amount of money in the bank account for the player that
    [p] represents. *)

val get_owned_properties : t -> Position.square list
(** [get_owned_properties p] is the list of owned properties of player [p]
    categorized by the string titles of the tiles*)

val current_location : t -> Position.square
(** [current_location p] is the current board position of the player [p]. *)

(*_______Functions that return a changed player_______*)

val move : int -> Position.t -> t -> t
(** [move x b p] is the new position of the player [p] after rolling the die. *)

val tile_owned : t -> Position.square -> bool
(** [tile_owned pl pr] returns true if the property [pr] is owned by player [pl]*)

val buy_property : Position.square -> t -> t
(** [buy_property pr pl] adds the property square representation [pr] to the
    purchased properties of player [pl]*)

val rails_owned : Position.t -> t -> int
(*[rails_owned b pl] is the number of railroad squares owned by player [pl] in
  board [b] *)

val util_owned : Position.t -> t -> int
(*[rails_owned b pl] is the number of utility squares owned by player [pl] in
  board [b] *)

exception Broke

val deposit : int -> t -> t
(** [deposit i pl] deposits [i] dollars into the account of the player [pl]*)

val withdraw : int -> t -> t
(** [withdraw i pl] removes [i] dollars from the account of the player [pl]. If
    [pl] has less than [i] dollars in their account then raises Broke. *)
