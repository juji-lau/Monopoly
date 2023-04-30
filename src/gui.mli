val width : int
val height : int

val setup : unit -> unit
(** [setup] sets up the initial window *)

val draw_intro : unit -> unit
(** [draw_intro] draws the introduction window *)

val print_player_stats : Player.t -> unit
(** [print_stats player] prints the name, balance, position, and properties
    owned of the current player *)

val player_position : Player.t -> unit
(** [player_posiiton player color] draws a circle representing the player,
    [player] on the board with the color [color]. *)

val draw_player_window : Player.t -> Raylib.Color.t -> unit
(** [draw_player_window player color] draws the window showing the state of the
    [player] *)

val draw_exit : unit -> unit
(** [draw_exit] draws the exit window, when a player types "Quit" into the
    terminal. *)

val loop : unit -> unit
