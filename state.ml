open OScrabbl

exception InvalidTilePlacement
exception InvalidWords
exception SingleLetter

(** [repeat f n st] applies [f] to [st] [n] times. *)
let rec repeat f n st = 
  if n > 0 then repeat f (n-1) (f st)
  else st

type state = {
  board: board;
  player_hand: letter list;
  bot_hand: letter list;
  letter_bag: letter array;
  coords: (int * int) list;
  available_letters: char list;
  checked_words: (string, unit) Hashtbl.t;
  history: (string * string * int) list;
  player_turn: bool;
  player_score: int;
  bot_score: int;
}

(** [set_double_letters b] sets the appropriate tiles of [b] to be
    double letter tiles. *)
let set_double_letters b =
  (* Grouped by tiles that correspond to each other, for readability *)
  b.(0).(3) <- double_letter; b.(0).(11) <- double_letter;
  b.(2).(6) <- double_letter; b.(2).(8) <- double_letter;
  b.(3).(0) <- double_letter; b.(3).(7) <- double_letter; 
  b.(3).(14) <- double_letter;
  b.(6).(2) <- double_letter; b.(6).(6) <- double_letter; 
  b.(6).(8) <- double_letter; b.(6).(12) <- double_letter;
  b.(7).(3) <- double_letter; b.(7).(11) <- double_letter; 
  b.(8).(2) <- double_letter; b.(8).(6) <- double_letter; 
  b.(8).(8) <- double_letter; b.(8).(12) <- double_letter;
  b.(11).(0) <- double_letter; b.(11).(7) <- double_letter; 
  b.(11).(14) <- double_letter;
  b.(12).(6) <- double_letter; b.(12).(8) <- double_letter;
  b.(14).(3) <- double_letter; b.(14).(11) <- double_letter

(** [set_double_letters b] sets the appropriate tiles of [b] to be
    double word tiles. *)
let set_double_words b = 
  b.(1).(1) <- double_word;
  b.(2).(2) <- double_word;
  b.(3).(3) <- double_word;
  b.(4).(4) <- double_word; 
  b.(10).(10) <- double_word;
  b.(11).(11) <- double_word;
  b.(12).(12) <- double_word;
  b.(13).(13) <- double_word;
  b.(13).(1) <- double_word;
  b.(12).(2) <- double_word;
  b.(11).(3) <- double_word;
  b.(10).(4) <- double_word; 
  b.(4).(10) <- double_word;
  b.(3).(11) <- double_word;
  b.(2).(12) <- double_word;
  b.(1).(13) <- double_word

(** [set_double_letters b] sets the appropriate tiles of [b] to be
    triple word tiles. *)
let set_triple_words b = 
  b.(0).(0) <- triple_word;
  b.(0).(7) <- triple_word;
  b.(0).(14) <- triple_word;
  b.(7).(0) <- triple_word;
  b.(7).(14) <- triple_word;
  b.(14).(0) <- triple_word;
  b.(14).(7) <- triple_word;
  b.(14).(14) <- triple_word

(** [set_double_letters b] sets the appropriate tiles of [b] to be
    triple letter tiles. *)
let set_triple_letters b = 
  b.(1).(5) <- triple_letter;
  b.(1).(9) <- triple_letter;
  b.(5).(1) <- triple_letter;
  b.(5).(5) <- triple_letter;
  b.(5).(9) <- triple_letter;
  b.(5).(13) <- triple_letter;
  b.(9).(1) <- triple_letter;
  b.(9).(5) <- triple_letter;
  b.(9).(9) <- triple_letter;
  b.(9).(13) <- triple_letter;
  b.(13).(5) <- triple_letter;
  b.(13).(9) <- triple_letter

(** [set_origin b] sets the origin tile at (7,7) as a special tile where 
    every OScrabbl game must start. *)
let set_origin b = b.(7).(7) <- origin_letter

(** [set_board_modifiers b] sets the appropriate tiles of [b] to be 
    to be score multiplier tiles. *)
let set_board_modifiers (b : board) : unit =
  set_double_words b;
  set_triple_words b; 
  set_double_letters b;
  set_triple_letters b;
  set_origin b

(** [init_board] is the initial board state of every game. *)
let init_board () : board = 
  let arr = Array.make_matrix 15 15 empty_tile in 
  set_board_modifiers (arr); 
  arr

let index c = Char.code c - 65

let index' n = Char.chr (n + 65)

let init_bag () = 
  let bag = Array.of_list letter_qty in 
  bag (* unnecessary, but works around OCaml and Reason linting issue *)

(** [add_to_bag ch st] updates the bag by incrementing the count of [ch]. *)
let add_to_bag char st : unit = 
  let len = Array.length st.letter_bag in
  let idx = if char = ' ' then len - 1 else index char in
  let letter_entry = Array.get st.letter_bag idx in
  let letter_entry' = 
    match letter_entry with
    | (c, num) -> (c, num + 1) in
  let _ = Array.set st.letter_bag idx letter_entry' in () (* for lint issue *)

(** [take_from_bag ch st] updates the bag by decrementing the count of [ch], 
    and returns whether that letter still remains in the bag. *)
let take_from_bag char st : bool =
  let len = Array.length st.letter_bag in
  let idx = if char = ' ' then len - 1 else index char in
  let letter_entry = Array.get st.letter_bag idx in 
  let letter_entry' = 
    match letter_entry with
    | (c, num) -> (c, num - 1) in
  Array.set st.letter_bag idx letter_entry'; 
  let still_in_bag = snd letter_entry' <> 0 in still_in_bag (* for clarity *)

let update_available_letters removing char st : state =
  if removing then 
    let still_in_bag = take_from_bag char st in 
    if still_in_bag then st
    else 
      let avail = List.filter (fun c -> c <> char) st.available_letters in 
      let st' = { 
        st with
        available_letters = avail
      } in st'
  else 
    (add_to_bag char st;
     if List.exists (fun c -> c = char) st.available_letters then st
     else 
       let avail = char::st.available_letters in 
       let st' = {
         st with available_letters = avail
       } in st')

(** [letter_of_char char] finds the letter corresponding to [char]. *)
let letter_of_char char = 
  List.find (fun e -> fst e = char) bucket

(** [add_letter_to_hand c hand] adds the corresponding letter of [c] 
    to [hand]. Should only be called if the letter is removed elsewhere. *)
let add_letter_to_hand char hand : letter list = 
  let letter = letter_of_char char in 
  let new_hand = letter::hand in 
  new_hand (* unnecessary, but works around OCaml and Reason linting issue *)

(** [remove_letter_from_hand c hand] removes the corresponding letter of [c] 
    from [hand]. Should only be called if the letter is added elsewhere. *)
let rec remove_letter_from_hand char hand : letter list = 
  let letter = letter_of_char char in
  match hand with 
  | [] -> raise Not_found
  | h::t -> if h = letter then t 
    else h::(remove_letter_from_hand char t)

(** [update_hand f st] is the game state with the current player's hand 
    updated by applying [f]. *)
let update_hand f st = 
  if st.player_turn then {
    st with 
    player_hand = f (st.player_hand)
  } else {
    st with 
    bot_hand = f (st.bot_hand)
  }

(** [init_available_letters] is the list of characters that are in the bag 
    at the start of the game. *)
let init_available_letters = List.map fst bucket

let rec fill_hand st : state = 
  let hand = if st.player_turn then st.player_hand else st.bot_hand in
  if List.length hand = 7 || st.available_letters == [] then st
  else 
    let len_avail = List.length st.available_letters in 
    Random.self_init ();
    let rand = Random.int len_avail in 
    let random_char = List.nth st.available_letters rand in 
    st |> update_available_letters true random_char 
    |> update_hand (add_letter_to_hand random_char) |> fill_hand

let use_letter char st : state = 
  update_hand (remove_letter_from_hand char) st

let put_on_board wc_option (x,y) c st = 
  let letter = letter_of_char c in 
  let tile = st.board.(x).(y) in 
  if tile.status = Set || tile.status = Filled then raise InvalidTilePlacement
  else
    let letter' = 
      if wc_option = None then letter else (fst letter, 0) in 
    let tile' = {
      tile with 
      status = Filled;
      letter = letter'
    } in 
    st.board.(x).(y) <- tile';
    {
      st with
      coords = (x,y)::st.coords
    }

(** [is_set (x,y) b] is if the tile at [(x,y)] on [b] is [Set]. *)
let is_set (x,y) b = b.(x).(y).status = Set

let adjacent_squares (i,j) = 
  let output = ref [] in 
  if i + 1 <= 14 then output := (i+1, j)::!output;
  if i - 1 >= 0 then output := (i-1, j)::!output;
  if j + 1 <= 14 then output := (i, j+1)::!output;
  if j - 1 >= 0 then output := (i, j-1)::!output;
  !output

(** [checkable_coords acc coord] is the set of tiles that can be checked
    adjacent to the tile at [coord], combined with [acc]. Used for folding. *)
let checkable_coords accum coord =
  let rec loop acc = function
    | [] -> acc
    | h::t -> if List.mem h acc then loop acc t else loop (h::acc) t in 
  loop accum (adjacent_squares coord)

let is_row lst b = 
  let rec is_connected coords board =
    let adj_tiles = coords |> List.fold_left checkable_coords [] in 
    let rec check_adj_tiles = function
      | [] -> false
      | h::t -> if is_set h b then true else check_adj_tiles t in 
    check_adj_tiles adj_tiles in 
  match lst with 
  | [] -> raise InvalidTilePlacement
  | [_] -> 
    if List.mem (7,7) lst || is_connected lst b then true
    else raise InvalidTilePlacement
  | h::t -> 
    let is_row_placement x xl = List.for_all (fun elt -> fst elt = x) xl in 
    let is_col_placement y yl = List.for_all (fun elt -> snd elt = y) yl in 
    let is_r = is_row_placement (fst h) t in 
    if (is_r <> is_col_placement (snd h) t) && is_connected lst b then is_r 
    else if List.mem (7,7) lst then is_r 
    else raise InvalidTilePlacement

(** [string_of_tile_list tlst] is the string of the letters in [tlst]. *)
let string_of_tile_list tlst = 
  let string_of_tile str tile =
    str ^ (tile.letter |> fst |> Char.escaped) in 
  List.fold_left string_of_tile "" tlst

(** [check_word st tlst] is whether the English word formed by the letters of 
    [tlst], in order of reverse-insertion (from left to right). *)
let check_word st tlst = 
  if tlst = [] then true 
  else 
    let word = string_of_tile_list tlst in 
    if Hashtbl.mem st.checked_words word then true 
    else if valid_words |> Lexicon.mem word 
    then (Hashtbl.add st.checked_words word (); true)
    else false

(** [column_word b coord] checks a column of tiles if a word was formed by tile 
    placement on [b], then returns the word as a [tile list]. *)
let column_word b (x,y) =
  let rec column_up x y b acc = 
    if x < 0 || b.(x).(y).status = Empty then acc 
    else column_up (x-1) y b (b.(x).(y)::acc) in
  let rec column_down x y b acc = 
    if x > 14 || b.(x).(y).status = Empty then acc 
    else column_down (x+1) y b (acc @ [b.(x).(y)]) in 
  let wlst = column_up x y b [] @ column_down (x+1) y b [] in 
  if List.length wlst <= 1 then [] else wlst

(** [row_word b coord] checks a row of tiles if a word was formed by tile 
    placement on [b], then returns the word as a [tile list]. *)
let row_word b (x,y) = 
  let rec row_left x y b acc = 
    if y < 0 || b.(x).(y).status = Empty then acc 
    else row_left x (y-1) b (b.(x).(y)::acc) in 
  let rec row_right x y b acc = 
    if y > 14 || b.(x).(y).status = Empty then acc 
    else row_right x (y+1) b (acc @ [b.(x).(y)]) in 
  let wlst = row_left x y b [] @ row_right x (y+1) b [] in 
  if List.length wlst <= 1 then [] else wlst

(** [score_of_word bonus score tlst] is the score of the word given by [tlst], 
    with initial [score] of 0 and a [bonus] of 1 for regular usage. *)
let rec score_of_word mult acc tlst =
  match tlst with 
  | [] -> mult * acc
  | h::t -> begin 
      match h.modifier with 
      | Nil | Origin -> score_of_word mult (acc + snd h.letter) t
      | Char n -> score_of_word mult (acc + n * (snd h.letter)) t
      | Word n -> score_of_word (mult * n) (acc + snd h.letter) t
    end

(** [choose lst] takes the first non-empty list element from [lst]. 
    Raises: [SingleLetter] if only a single letter was placed at the very 
    first turn of the game. *)
let rec choose = function 
  | [] -> raise SingleLetter
  | h::t -> if h = [] then choose t else h

let score_move st =
  let word_list = 
    if is_row st.coords st.board then 
      row_word st.board (List.hd st.coords)::List.map (column_word st.board) 
        st.coords 
    else column_word st.board (List.hd st.coords)::List.map (row_word st.board) 
           st.coords in 
  if List.for_all (check_word st) word_list then 
    (List.fold_left (
        fun acc word -> acc + score_of_word 1 0 word
      ) 0 word_list, string_of_tile_list (choose word_list))
  else raise InvalidWords

let reset_coords st = {
  st with 
  coords = []
}

let set_board st = 
  let b = st.board in 
  let coords = st.coords in 
  List.iter (
    fun c -> 
      let tile = b.(fst c).(snd c) in 
      b.(fst c).(snd c) <- {
        tile with 
        status = Set;
        modifier = Nil;
      } 
  ) coords

let reset_board st = 
  let reset_tile (x,y) = 
    let old_tile = st.board.(x).(y) in 
    let new_tile = {
      old_tile with 
      status = Empty;
      letter = (' ', 0)
    } in 
    st.board.(x).(y) <- new_tile in 
  List.iter reset_tile st.coords

(** [board_to_hand coords st] is a helper function to put every [Filled] letter 
    from the board back to the current player's hand. *)
let rec board_to_hand state =
  let rec letters_from_coords acc st = function
    | [] -> acc
    | (i,j)::t -> 
      let letter = st.board.(i).(j).letter in
      let new_letter = if snd letter = 0 then (' ', 0) else letter in 
      letters_from_coords (new_letter::acc) st t in 
  let retrieved_letters = letters_from_coords [] state state.coords in 
  update_hand (fun hand -> hand @ retrieved_letters) state

let put_everything_back st = 
  let st' = board_to_hand st in 
  reset_board st'; reset_coords st'

let set_history pl wd s st = 
  if List.length st.history < 4 then {
    st with
    history = (pl, wd, s)::st.history
  } else
    let shift_list lst = 
      let rec loop acc = function
        | [] -> acc
        | [e] -> acc
        | h::t -> loop (h::acc) t in 
      loop [] lst |> List.rev in {
      st with 
      history = (pl, wd, s)::(shift_list st.history)
    }

let confirm_turn st = 
  let updated_score_and_int = score_move st in 
  let score = fst updated_score_and_int in 
  let word = snd updated_score_and_int in 
  if st.player_turn then (
    let st' = {
      st with 
      player_score = score + st.player_score;
    } in set_board st'; st' |> reset_coords |> set_history "Player" word score
  ) else (
    let st' = {
      st with 
      bot_score = score + st.bot_score;
    } in set_board st'; st' |> reset_coords |> set_history "Bot" word score
  )

let pass_turn st = {
  st with 
  player_turn = not st.player_turn
}

let init_state () : state = 
  Random.self_init ();
  {
    board = init_board ();
    player_hand = [];
    bot_hand = [];
    letter_bag = init_bag ();
    coords = [];
    available_letters = init_available_letters;
    checked_words = Hashtbl.create 20;
    history = [];
    player_turn = Random.bool ();
    player_score = 0;
    bot_score = 0;
  }

let game_start () = init_state () |> fill_hand |> pass_turn |> fill_hand