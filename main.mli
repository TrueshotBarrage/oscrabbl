(** 
   The main entry point for the game interface.
*)

open Scrabble

val check_file : string -> string

(** [print_board b] pretty-prints a game board [b]. *)
val print_board : board -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit