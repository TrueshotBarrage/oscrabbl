open State
open Scrabble

(** [pp_color c str] pretty-prints a string [str] using color [c]. *)
let pp_color c = ANSITerminal.print_string [c]

let pp_y = pp_color ANSITerminal.yellow

let pp_b = pp_color ANSITerminal.cyan

let pp_m = pp_color ANSITerminal.magenta

let pp_w = pp_color ANSITerminal.white

let pp_r = pp_color ANSITerminal.red

let pp_g = pp_color ANSITerminal.green

let pp_bl = pp_color ANSITerminal.blue

let pp_rainbow str = 
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [] in 
  let rainbow_list = [pp_r; pp_y; pp_g; pp_b; pp_bl; pp_m; pp_w] in 
  let ch_list = explode str in 
  let rec pp_rainbow_aux i = function
    | [] -> ()
    | h::t -> h |> Char.escaped |> List.nth rainbow_list i; 
      if i < 6 then pp_rainbow_aux (i+1) t else pp_rainbow_aux 0 t in 
  pp_rainbow_aux 0 ch_list

(** [print_hand_tile row ch num] pretty-prints a tile of [ch] and [num] value 
    in a player's hand. [row] determines which part of the tile to print. *)
let print_hand_tile row ch num = 
  if row = 0 then print_string      " ___  "
  else if row = 1 then 
    (print_string "|"; 
     (if ch <> ' ' then ch |> Char.escaped |> pp_w else pp_w "*"); 
     print_string "  | ")
  else if row = 2 then 
    (print_string "|_";
     if num < 10 then print_string "_";
     num |> string_of_int |> pp_w; print_string "| ")

(** [print_player_hand i st] pretty-prints the player's hand in [st]. *)
let print_player_hand i st =
  if i = 0 then print_string "              PLAYER'S HAND              "
  else if i = 1 then print_string "-----------------------------------------"
  else let hand = st.player_hand |> List.split in 
    let chars = fst hand in let nums = snd hand in 
    if i = 2 then List.iter (print_hand_tile 0 ' ') nums
    else if i = 3 then List.iter (fun ch -> print_hand_tile 1 ch 0) chars
    else if i = 4 then List.iter (fun n -> print_hand_tile 2 ' ' n) nums 

(** [print_bot_hand i st] pretty-prints the bot's hand in [st]. *)
let print_bot_hand i st =
  let offset = 8 in 
  if i = offset then print_string "                BOT'S HAND               "
  else if i = 1 + offset 
  then print_string "-----------------------------------------"
  else let hand = st.bot_hand |> List.split in 
    let chs = fst hand in let nms = snd hand in 
    if i = 2 + offset then List.iter (print_hand_tile 0 ' ') nms
    else if i = 3 + offset then List.iter (fun ch -> print_hand_tile 1 ch 0) chs
    else if i = 4 + offset then List.iter (fun n -> print_hand_tile 2 ' ' n) nms 

(** [print_history i st] pretty-prints the history of played words in [st]. *)
let print_history i st =
  let offset = 16 in 
  if i = offset then print_string "                 HISTORY                 " 
  else if i = 1 + offset 
  then print_string "-----------------------------------------" 
  else if i >= 2 + offset then 
    List.iteri (
      fun row (pl,wd,s) -> if row * 2 + offset + 2 = i then (
          print_string ">  "; pp_b pl; pp_w " played "; pp_m "\""; pp_m wd; 
          pp_m "\""; pp_w " for "; s |> string_of_int |> pp_y; pp_w " points!"
        )) st.history

(** [print_scores i st] pretty-prints the score of each player in [st]. *)
let print_scores i st = 
  if i = 27 then print_string "                  SCORE                  "
  else if i = 28 then print_string "-----------------------------------------"
  else if i = 29 then (
    pp_b "  Player: "; st.player_score |> string_of_int |> pp_g; 
    pp_g " points  "; pp_w "<>"; 
    pp_b "  Bot: "; st.bot_score |> string_of_int |> pp_r; pp_r " points  ")

(** [print_misc i st] prints everything other than the board in [st]. *)
let print_misc i st = 
  if i <= 4 then print_player_hand i st
  else if i >= 8 && i <= 12 then print_bot_hand i st
  else if i >= 16 && i <= 24 then print_history i st
  else if i >= 27 then print_scores i st

(** [print_tile_top t] pretty-prints [t]'s upper half, ASCII style. *)
let print_tile_top t = 
  match t.status with 
  | Empty -> begin
      match t.modifier with 
      | Nil -> pp_y "   |"
      | Word i -> 
        print_string ((string_of_int i) ^ "WD"); 
        pp_y "|"
      | Char i -> print_string ((string_of_int i) ^ "LT");
        pp_y "|"
      | Origin -> pp_b "***";
        pp_y "|"
    end
  | Filled -> 
    (t.letter |> fst |> Char.escaped) ^ "  " |> pp_m;
    pp_y "|"
  | Set -> 
    (t.letter |> fst |> Char.escaped) ^ "  " |> pp_w;
    pp_y "|"

(** [print_tile_top t] pretty-prints [t]'s lower half, ASCII style. *)
let print_tile_bottom t = 
  match t.status with 
  | Empty -> pp_y "___|"
  | Filled -> begin
      let value = t.letter |> snd in 
      let print_value () = 
        if value > 9 then pp_m (string_of_int value) 
        else (pp_y "_"; pp_m (string_of_int value)) in 
      pp_y "_";
      print_value (); 
      pp_y "|"
    end
  | Set -> begin
      let value = t.letter |> snd in 
      let print_value () = 
        if value > 9 then pp_w (string_of_int value) 
        else (pp_y "_"; pp_w (string_of_int value)) in 
      pp_y "_"; 
      print_value ();
      pp_y "|"
    end

(** [print_row pad st i arr] pretty-prints a row of a tiles array of [arr]. *)
let print_row pad st i arr = 
  print_string pad;
  i |> string_of_int |> pp_b;
  if i < 10 then print_string " " else ();
  pp_y " |";
  Array.iter print_tile_top arr;
  print_string " ";
  if i < 10 then print_string " " else ();
  i |> string_of_int |> pp_b;
  print_string pad;
  print_misc (i * 2) st;
  print_string "\n";
  print_string pad;
  pp_y "   |";
  Array.iter print_tile_bottom arr;
  print_string ("   " ^ pad);
  print_misc (i * 2 + 1) st;
  print_string "\n"

let print_board st = 
  let padding = "      " in 
  print_string padding;
  pp_b "     0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  \n";
  print_string padding;
  pp_y "    ___________________________________________________________ \n";
  Array.iteri (print_row padding st) st.board;
  print_string "\n";
  print_string padding;
  pp_b "     0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  \n"

let print_turn_prompt st = 
  pp_w "\nIt is the "; 
  if st.player_turn then pp_b "Player" else pp_b "Bot"; pp_w "'s turn. ";
  if st.player_turn then pp_m "What do you want to do?\n" else pp_w "\n"