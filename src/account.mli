(** Representation of player bank account.

    This module represents the data stored in the account of each player. It
    handles buying properties and houses, paying taxes and recieving money. *)

type t
(** The abstract value of a players bank account. *)

exception Broke
(** Raise Broke when a player has less in their account than they are expected
    to pay *)

val init : t
(** [init] initializes a new account with 1500 dollars. *)

val current : t -> int
(** [current a] gives the current integer amount in account [a]. *)

val pay : int -> t -> t
(** [pay i a] removes [i] dollars from account [a]. If [a] has less than [i]
    dollars in their account then raises Broke. Requires: [i] >= 0*)

val receive : int -> t -> t
(** [ receive i a ] adds [i] dollars to account [a]. Requires: [i] >= 0*)
