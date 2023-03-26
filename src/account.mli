(** Representation of player bank account. 

    This module represents the data stored in the account of each player.
    It handles buying properties and houses, paying taxes and recieving money. *)

type t 
(** The abstract value of a players bank account. *)

val init : t
(** [init] initializes a new account with 1500 dollars. *)

val current : t -> int
(** [current a] gives the current integer amount in account [a]. *)

val pay : int -> t -> t
(** [pay i a] removes [i] dollars from account [a]. If [a] has less than [i] dollars in 
    their account then raises Broke. *)

val recieve : int ->t -> t
(** [ recieve i a ] adds [i] dollars to account [a]. *)
