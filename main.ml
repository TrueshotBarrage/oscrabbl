let rec check_file f =
  failwith "unimplemented"

open State
open Scrabble

(** [pp_color c str] pretty-prints a string [str] using color [c]. *)
let pp_color c = ANSITerminal.print_string [c]

(** [pp_y str] is a shorthand for [pp_color ANSITerminal.yellow str]. *)
let pp_y = pp_color ANSITerminal.yellow

(** [pp_b str] is a shorthand for [pp_color ANSITerminal.cyan str]. *)
let pp_b = pp_color ANSITerminal.cyan

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
    (t.letter |> fst |> Char.escaped) |> print_string;
    pp_y "|"
  | Set -> 
    (t.letter |> fst |> Char.escaped) ^ "  " |> print_string;
    pp_y "|"

(** [print_tile_top t] pretty-prints [t]'s lower half, ASCII style. *)
let print_tile_bottom t = 
  match t.status with 
  | Empty -> pp_y "___|"
  | Filled -> begin
      let value = t.letter |> snd in 
      let print_value = 
        if value > 9 then print_string (string_of_int value) 
        else pp_y "_";
        print_string (string_of_int value) in 
      pp_y "_";
      print_value; 
      pp_y "|"
    end
  | Set -> begin
      let value = t.letter |> snd in 
      let value_str = 
        if value > 9 then string_of_int value else "_" ^ string_of_int value in 
      "_" ^ value_str ^ "|" |> print_string
    end

(** [print_row arr] pretty-prints an entire row of a tiles array [arr]. *)
let print_row i arr = 
  i |> string_of_int |> pp_b;
  let _ = if i < 10 then print_string " " else () in
  pp_y " |";
  Array.iter print_tile_top arr;
  print_string " ";
  let _ = if i < 10 then print_string " " else () in
  i |> string_of_int |> pp_b;
  pp_y "\n   |";
  Array.iter print_tile_bottom arr;
  print_string "\n"

let print_board b = 
  pp_b "     0   1   2   3   4   5   6   7   8   9   10  11  12  13  14 \n";
  pp_y "   _____________________________________________________________\n";
  Array.iteri print_row b;
  pp_b "\n     0   1   2   3   4   5   6   7   8   9   10  11  12  13  14 \n"

let main () =
  let init = init_state in 
  print_board init.board

(* Execute the game engine. *)
let () = main ()
