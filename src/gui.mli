val width : int
val height : int

val setup : unit -> unit
(** [setup] sets up the initial window *)

val draw_intro : unit -> unit
(** [draw_intro] draws the introduction window *)

val print_player_stats : Player.t -> unit
(** [print_stats player] prints the name, balance, position, and properties
    owned of the current player *)

val player_position : Player.t -> Player.t -> unit
(** [player_posiiton player1 player2 color] draws a circle representing the
    players, [player1] and [player2] on the board with their respective colors.
    [player1] is the current player. *)

val draw_player_window : Player.t -> Player.t -> unit
(** [draw_player_window player1 player2 color] draws the window showing the
    state of the [player1] and the location of [player2]. [player1] is the
    current player. *)

val draw_exit : unit -> unit
(** [draw_exit] draws the exit window, when a player types "Quit" into the
    terminal. *)

val loop : unit -> unit
