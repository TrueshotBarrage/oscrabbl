(** 
   Helper functions used to pretty-print board elements and feedback messages 
   quickly and beautifully.
*)
open State
open OScrabbl
open ANSITerminal

(** [pp_color c str] pretty-prints a string [str] using color [c]. *)
val pp_color : style -> string -> unit

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

(** [pp_bl str] is a shorthand for [pp_color ANSITerminal.blue str]. *)
val pp_bl : string -> unit

(** [pp_rainbow str] pretty-prints [str] with rainbow colors. *)
val pp_rainbow : string -> unit

(** [print_board st] pretty-prints a game board in [st]. *)
val print_board : state -> unit

(** [print_turn_prompt st] prints a prompt, indicating whose turn it is in [st],
    and if it is the player's turn, asks what the player will do. *)
val print_turn_prompt : state -> unit