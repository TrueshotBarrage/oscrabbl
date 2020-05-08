(** 
   The main entry point for the game interface.
*)
open Command
open Printer
open OScrabbl
open State

(** [update_state cmd st] is a helper function that [continue] calls to 
    update the current state [st] of the game. It handles both good and bad 
    commands [cmd] and returns the correct corresponding state, as well as 
    printing helpful feedback for the user. *)
val update_state : string -> state -> state

(** [continue st] recursively keeps the game going by updating the state, 
    flushing and printing the board, then reprompting the user. *)
val continue : state -> unit

(** [main ()] starts OScrabbl! *)
val main : unit -> unit