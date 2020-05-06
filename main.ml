open State
open Scrabble
open Printer

let rec continue st = 
  match read_line () with
  | exception End_of_file -> ()
  | cmd -> begin
      let st' = st in 
      let _ = Sys.command "clear" in 
      print_endline " ";
      print_board st';
      print_string "> ";
      continue st'
    end

(** [test0 st] is a new state with a test set of actions applied to [st]. *)
let test0 st = 
  st |> put_on_board (7,7) 'H' |> put_on_board (7,8) 'E' 
  |> put_on_board (7,9) 'Y' |> confirm_player_turn

(** [test1 st] is a new state with a test set of actions applied to [st]. *)
let test1 st = 
  st |> put_on_board (8,7) 'E' |> put_on_board (9,7) 'L'
  |> put_on_board (10,7) 'L' |> put_on_board (11,7) 'O' |> confirm_bot_turn

(** [test2 st] is a new state with a test set of actions applied to [st]. *)
let test2 st = 
  st |> put_on_board (6,8) 'Y' |> put_on_board (8,8) 'S' |> confirm_player_turn

(** [test3 st] is a new state with a test set of actions applied to [st]. *)
let test3 st = 
  st |> put_on_board (8,6) 'Y' |> put_on_board (9,6) 'E' 
  |> put_on_board (10,6) 'A' |> put_on_board (11,6) 'H' |> confirm_bot_turn

(** [test4 st] is a new state with a test set of actions applied to [st]. *)
let test4 st = 
  st |> put_on_board (11,8) 'W' |> put_on_board (11,9) 'D' 
  |> put_on_board (11,10) 'Y' |> confirm_player_turn

(** [test5 st] is a new state with a test set of actions applied to [st]. *)
let test5 st = st |> fill_player_hand |> fill_bot_hand

let main () =
  ANSITerminal.resize 80 40;
  let st0 = init_state () in
  let st1 = test0 st0 in 
  let st2 = test1 st1 in 
  let st3 = test2 st2 in 
  let st4 = test3 st3 in 
  let st5 = test4 st4 in 
  let st = test5 st5 in 
  print_board st;
  flush stdout;
  continue st

(* Execute the game engine. *)
let () = main ()
