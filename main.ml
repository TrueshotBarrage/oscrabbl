open Command
open Printer
open Scrabble
open State

let pass_counter = ref 0

(** [put wc_opt ch (i,j) st] updates [st] with the "put" command to put [ch] 
    at [(i,j)], possibly with the wildcard "*" tile. *)
let put wc_opt ch (i,j) st = 
  let ch' = if wc_opt = None then ch else ' ' in 
  match use_letter ch' st with 
  | exception Not_found -> pp_r "You don't have "; 
    if ch' = ' ' then pp_y "*" else ch' |> Char.escaped |> pp_y; 
    pp_r " in your hand."; st
  | st' -> begin
      match put_on_board wc_opt (i,j) ch st' with 
      | exception InvalidTilePlacement -> 
        pp_r "That square is already taken by another tile!"; st
      | st'' -> st''
    end

(** [exchange ch_list st] updates [st] with the "exchange" command to exchange 
    all the letters in [ch_list] from the bag in [st]. *)
let exchange ch_list st = 
  let rec remove_ls_from_hand lst state = 
    match lst with  
    | [] -> state
    | h::t -> begin
        let h = if h = '*' then ' ' else h in
        match use_letter h state with 
        | exception Not_found -> 
          pp_r ("You don't have " ^ Char.escaped h ^ " in your hand!"); st
        | st' -> remove_ls_from_hand t st'
      end in 
  let st' = remove_ls_from_hand ch_list st in 
  if st = st' then st else (
    pass_counter := 0;
    List.fold_left (
      fun state ch -> update_available_letters false ch state
    ) st' ch_list |> fill_hand |> pass_turn
  )

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
  pp_w "Welcome to "; pp_rainbow "OScrabbl"; 
  pp_w 
    "! OScrabbl is a REPL-style Scrabble game designed for the command line int\
     erface.\nThe rules are almost identical to real-life Scrabble.\n\n\n";
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
  if true then () else ();
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
      | exception SingleLetter -> 
        pp_r "You need to place a valid word with at least two characters!"; st
      | st' -> pass_counter := 0; st' |> fill_hand |> pass_turn
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
    else (pp_r "Exchange should only take single letters!"; st)
  | Help -> print_help st; st
  | Pass -> pass_counter := !pass_counter + 1;
    st |> put_everything_back |> pass_turn
  | Quit -> pp_y "Thanks for playing!\n"; exit 0 

let rec continue st = 
  match read_line () with
  | exception End_of_file -> ()
  | cmd -> begin
      let st' = update_state cmd st in 
      let _ = Sys.command "clear" in 
      print_endline " ";
      if st.player_hand = [] && st.bot_hand = [] || !pass_counter = 6 
      then 
        if st'.player_score = st'.bot_score 
        then (pp_w "It's a tie. No one wins this round!"; flush stdout)
        else (
          let winner, score = 
            if st'.player_score > st'.bot_score 
            then "Player", string_of_int st'.player_score 
            else "Bot", string_of_int st'.bot_score in
          pp_b winner; pp_rainbow " wins the game with ";
          pp_g score; pp_rainbow " points! Congratulations!"; print_endline ""
        )
      else (
        if !pass_counter = 4 
        then pp_g 
            "Both players have passed twice in a row. If both players pass agai\
             n without making a meaningful move, the game will end!\n"
        else if !pass_counter = 5 
        then pp_g
            "Both players have passed twice in a row. If you pass now, the gam\
             e will end!\n";
        print_board st';
        print_turn_prompt st';
        print_string "> ";
        flush stdout;
        continue st'
      )
    end

let main () =
  ANSITerminal.resize 125 40;
  let st = game_start () in
  let _ = Sys.command "clear" in 
  print_help st;
  let _ = Sys.command "clear" in 
  print_string "Welcome to "; pp_rainbow "OScrabbl"; print_string "! Type "; 
  pp_y "\"help\""; print_endline " if you ever need a refresher.";
  print_board st;
  print_turn_prompt st;
  print_string "> ";
  flush stdout;
  continue st

(* Execute the game engine. *)
let () = main ()
