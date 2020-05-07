open Command
open Printer
open Scrabble
open State

(** [put wc_opt ch (i,j) st] updates [st] with the "put" command to put [ch] 
    at [(i,j)], possibly with the wildcard "*" tile. *)
let put wc_opt ch (i,j) st = 
  match use_letter ch st with 
  | exception Not_found -> pp_r "You don't have "; 
    ch |> Char.escaped |> pp_r; pp_r " in your hand."; st
  | st' -> begin
      match put_on_board (i,j) ch st' with 
      | exception InvalidTilePlacement -> 
        pp_r "That square is already taken by another tile!"; st
      | st'' -> st''
    end

(** [exchange ch_list st] updates [st] with the "exchange" command to exchange 
    all the letters in [ch_list] from the bag in [st]. *)
let exchange ch_list st = failwith "Unimplemented"

(** [explode str] converts [str] to a [char list]. *)
let explode str =
  let rec exp i acc = if i < 0 then acc else exp (i - 1) (str.[i]::acc) in
  exp (String.length str - 1) []

(** [classify_put_cmd wc_opt l i' j' st] updates [st] with the "put" command 
    parameters [l], [i'], [j'], and possibly "*" for the wildcard character. *)
let classify_put_cmd (wc_opt : string option) l i' j' st = 
  let i_opt = int_of_string_opt i' in 
  let j_opt = int_of_string_opt j' in 
  let ch_list = l |> String.uppercase_ascii |> explode in 
  match ch_list, i_opt, j_opt with 
  | [ch], Some i, Some j -> 
    let idx = index ch in 
    if idx >= 0 && idx < 26 then 
      if i >= 0 && i < 15 && j >= 0 && j < 15 then put wc_opt ch (i,j) st 
      else (pp_r "Coordinates must be between 0 and 14 inclusive!"; st)
    else (pp_r "Can't put that on the board. It's not a valid letter!"; st)
  | _ -> pp_r {|Please enter a valid move. Example: "put A at 3 4"|}; st

(** [print_help st] toggles the help manual for the game. Using this while 
    the help manual is shown will close the manual. *)
let print_help st = 
  let _ = Sys.command "clear" in 
  print_endline "\n\n\n";
  pp_y 
    "Welcome to OScrabbl! OScrabbl is a REPL-style Scrabble game designed for t\
     he command line interface.\nThe rules are almost identical to real-life Sc\
     rabble.\n\n\n";
  pp_g "List of supported commands:\n";
  pp_y "\"Put "; pp_m "<letter>"; pp_y " at "; pp_m "<row> <col>"; pp_y "\" ";
  pp_b "attempts to place your tile of "; pp_m "<letter>"; 
  pp_b " at the coordinates of ("; pp_m "<row>"; pp_b ", "; pp_m "<col>"; 
  pp_b 
    ").\nThe tile must not already be occupied by another letter, and you mus\
     t have "; pp_m "<letter>"; pp_b " in your hand!\n\n";
  pp_y 
    "\"Put * as "; pp_m "<letter>"; pp_y " at "; pp_m "<row> <col>"; pp_y "\" ";
  pp_b 
    "is the same as the above, but allows you to play a wildcard as any lette\
     r of your choice.\n\n";
  pp_y "\"Confirm\" ";
  pp_b 
    "is used after putting tiles onto the board to confirm your tile placemen\
     t and to end your turn.\nIf successful, you will be awarded the the point\
     s for your word(s), and your hand will be refilled.\n\n";
  pp_y "\"Clear\" ";
  pp_b 
    "resets your turn, removing all your placed tiles from the board and puttin\
     g them back into your hand.\nYour turn does not end, and you can still ma\
     ke a move.\n\n";
  pp_y "\"Exchange "; pp_m "<letter1> <letter2>"; pp_y " ...\" ";
  pp_b
    "allows you to replace any number of your letters with new ones from the ba\
     g.\nHowever, be aware that exchanging your tiles will end your turn.\n\n";
  pp_y "\"Pass\" "; pp_b "skips your turn, without committing any action.\n\n";
  pp_y "\"Help\" "; pp_b "toggles this help manual.\n\n";
  pp_y "\"Quit\" "; pp_b "exits the game.\n\n";
  pp_w "Enter any key to close this help manual.\n\n\n";
  pp_g "Developed by "; pp_rainbow "Tim Tran"; 
  pp_g " and "; pp_rainbow "David Kim"; pp_g " for CS 3110 in Spring 2020.\n";
  flush stdout;
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let update_state cmd st : state = 
  match parse cmd with
  | exception Empty -> pp_r "You didn't type anything."; st 
  | exception Malformed -> 
    pp_r "Please enter a valid move. If you need help, type \"help\" for a lis\
          t of supported commands."; st 
  | Put (l::"at"::i'::[j']) -> classify_put_cmd None l i' j' st
  | Put ("*"::"as"::l::"at"::i'::[j']) -> classify_put_cmd (Some "*") l i' j' st
  | Put _ -> 
    pp_r "The \"put\" command wasn't understood correctly. If you need help, t\
          ype \"help\" for a list of supported commands."; st
  | Confirm -> begin
      match confirm_turn st with 
      | exception InvalidWords -> pp_r "Cannot make that move! ";
        pp_w "At least one of the words formed by that move is invalid."; st
      | exception InvalidTilePlacement -> pp_r "Invalid tile placement!"; st
      | st' -> st' |> fill_hand |> pass_turn
    end
  | Clear -> put_everything_back st
  | Exchange lst -> 
    if List.fold_left (fun b str -> b && String.length str = 1) true lst 
    && List.length lst <= 7
       (* Using [List.hd] is fine here; we checked every element has len 1 *)
    then let ch_list = List.map (
        fun str -> str |> String.uppercase_ascii |> explode |> List.hd
      ) lst 
      in exchange ch_list st 
    else pp_r "Exchange should only take single letters!"; st
  | Help -> print_help st; st
  | Pass -> st |> put_everything_back |> pass_turn
  | Quit -> pp_y "Thanks for playing!\n"; exit 0 

let rec continue st = 
  match read_line () with
  | exception End_of_file -> ()
  | cmd -> begin
      let st' = update_state cmd st in 
      let _ = Sys.command "clear" in 
      print_endline " ";
      print_board st';
      print_turn_prompt st';
      print_string "> ";
      flush stdout;
      continue st'
    end

(** [test0 st] is a new state with a test set of actions applied to [st]. *)
let test0 st = 
  st |> put_on_board (7,7) 'H' |> put_on_board (7,8) 'E' 
  |> put_on_board (7,9) 'Y' |> confirm_turn

(** [test1 st] is a new state with a test set of actions applied to [st]. *)
let test1 st = 
  st |> put_on_board (8,7) 'E' |> put_on_board (9,7) 'L'
  |> put_on_board (10,7) 'L' |> put_on_board (11,7) 'O' |> confirm_turn

(** [test2 st] is a new state with a test set of actions applied to [st]. *)
let test2 st = 
  st |> put_on_board (6,8) 'Y' |> put_on_board (8,8) 'S' |> confirm_turn

(** [test3 st] is a new state with a test set of actions applied to [st]. *)
let test3 st = 
  st |> put_on_board (8,6) 'Y' |> put_on_board (9,6) 'E' 
  |> put_on_board (10,6) 'A' |> put_on_board (11,6) 'H' |> confirm_turn

(** [test4 st] is a new state with a test set of actions applied to [st]. *)
let test4 st = 
  st |> put_on_board (11,8) 'W' |> put_on_board (11,9) 'D' 
  |> put_on_board (11,10) 'Y' |> confirm_turn

(** [test5 st] is a new state with a test set of actions applied to [st]. *)
let test5 st = st |> fill_hand |> pass_turn |> fill_hand

(** [test6 st] is a new state with a test set of actions applied to [st]. *)
let test6 st = st |> put_on_board (8,7) 'I' |> confirm_turn

let main () =
  ANSITerminal.resize 125 36;
  let st = init_state () |> fill_hand |> pass_turn |> fill_hand in
  let st1 = test0 st in 
  let st6 = test6 st1 in 
  (*  let st3 = test2 st2 in 
      let st4 = test3 st3 in 
      let st5 = test4 st4 in 
      let st = test5 st5 in  *)
  let _ = Sys.command "clear" in 
  print_endline "Welcome to OScrabbl! Currently in development :)";
  print_board st6;
  print_turn_prompt st6;
  print_string "> ";
  flush stdout;
  continue st6

(* Execute the game engine. *)
let () = main ()
