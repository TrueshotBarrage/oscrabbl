open State
open Scrabble
open ANSITerminal

(** [pp_y str] is a shorthand for [pp_color ANSITerminal.yellow str]. *)
val pp_y : string -> unit

(** [pp_b str] is a shorthand for [pp_color ANSITerminal.cyan str]. *)
val pp_b : string -> unit

(** [pp_m str] is a shorthand for [pp_color ANSITerminal.magenta str]. *)
val pp_m : string -> unit

(** [pp_w str] is a shorthand for [pp_color ANSITerminal.white str]. *)
val pp_w : string -> unit

(** [pp_r str] is a shorthand for [pp_color ANSITerminal.red str]. *)
val pp_r : string -> unit

(** [pp_g str] is a shorthand for [pp_color ANSITerminal.green str]. *)
val pp_g : string -> unit

(** [print_board st] pretty-prints a game board in [st]. *)
val print_board : state -> unit