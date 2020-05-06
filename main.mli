(** 
   The main entry point for the game interface.
*)
open State
open Scrabble
open Printer

(** [continue st] recursively keeps the game going by updating the state, 
    flushing and printing the board, then reprompting the user. *)
val continue : state -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit