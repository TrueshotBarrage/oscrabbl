(** 
   The main entry point for the game interface.
*)

(* You are free to add more code here. *)
(** [check_file f] tries to get the json data from [f]. If [f] is invalid, then 
    the exception is caught with an error message, after which the user is 
    prompted again to enter the correct file. *)
val check_file : string -> Yojson.Basic.t

(** [play_game f] starts the adventure in file [f]. *)
val play_game : string -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit