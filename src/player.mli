(** Representation of player data. 
    
This module represents the data stored for each player in the game
including their bank accounts and current location. It handles the rolling
of the die as well as buying and slling properties. *)

type t
(** The abstract type of of values representing players. *)

val new_player : string -> t
(** [new_player s] is the player that [s] represents. *)

val roll_die : int
(** [roll_die] is the random number [1,6] inclusive that occurs with
    probability 1/6. *)

val current_location : t -> int
(** [current_location p] is the current board position of the player [p]. *)

val move_to : t -> t
(** [move_to p] is the new position of the player [p] after rolling the 
    die. *)
