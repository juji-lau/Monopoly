(** Parsing of player commands. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly an object phrase. *)
type command =
  | Roll
  | Purchase
  | EndTurn
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command]. Requires: [str]
    contains only alphanumeric (A-Z, a-z, 0-9) and space characters (only ASCII
    character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is neither "quit" nor "roll". *)
