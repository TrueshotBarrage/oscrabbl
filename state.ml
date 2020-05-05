open Scrabble

exception InvalidTilePlacement
exception InvalidWords
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
  player_score: int;
  bot_score: int;
}

(** [set_double_letters b] sets the appropriate tiles of [b] to be
    double letter tiles. *)
let set_double_letters (b : board) =
  b.(0).(3) <- double_letter; b.(0).(11) <- double_letter;
  b.(2).(6) <- double_letter; b.(2).(8) <- double_letter;
  b.(3).(0) <- double_letter; b.(3).(7) <- double_letter; b.(3).(14) <- double_letter;
  b.(6).(2) <- double_letter; b.(6).(6) <- double_letter; b.(6).(8) <- double_letter; b.(6).(12) <- double_letter;
  b.(7).(3) <- double_letter; b.(7).(11) <- double_letter; 
  b.(8).(2) <- double_letter; b.(8).(6) <- double_letter; b.(8).(8) <- double_letter; b.(8).(12) <- double_letter;
  b.(11).(0) <- double_letter; b.(11).(7) <- double_letter; b.(11).(14) <- double_letter;
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

let set_origin b =
  b.(7).(7) <- origin_letter

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

(** [index c] is the 0-based index of [c] in the alphabet. 
    If [c] is the space character [' '] with ASCII code 32, return -33. *)
let index c = Char.code c - 65

(** [index' n] is the reverse operation of [index c]. 
    [index' -33] is the space character [' ']. *)
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

(** [update_available_letters removing c st] either: 
    1) takes the letter corresponding to [c] from the letter bag and 
    removes [c] from the list of available letters if the last letter 
    of [c] was taken from the bag, if [removing].
    Requires: The letter of [c] is in the list of available letters. 
    or: 
    2) adds the letter corresponding to [c] to the letter bag and adds 
    [c] to the list of available letters if [c] does not exist in it,
    if not [removing]. *)
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

(** [update_player_hand f st] is the game state with the player's hand 
    updated by applying [f]. *)
let update_player_hand f st = 
  {
    st with 
    player_hand = f (st.player_hand)
  }

(** [update_bot_hand f st] is the game state with the bot's hand 
    updated by applying [f]. *)
let update_bot_hand f st = 
  {
    st with 
    bot_hand = f (st.bot_hand)
  }

(** [init_available_letters] is the list of characters that are in the bag 
    at the start of the game. *)
let init_available_letters = List.map fst bucket

let rec fill_player_hand st : state =
  if List.length st.player_hand = 7 || st.available_letters == [] then st
  else 
    let len_avail = List.length st.available_letters in 
    Random.self_init ();
    let rand = Random.int len_avail in 
    let random_char = List.nth st.available_letters rand in 
    let st' = update_available_letters true random_char st in 
    let st'' = update_player_hand (add_letter_to_hand random_char) st' in 
    fill_player_hand st''

let rec fill_bot_hand st : state = 
  if List.length st.bot_hand = 7 || st.available_letters == [] then st
  else 
    let len_avail = List.length st.available_letters in 
    Random.self_init ();
    let rand = Random.int len_avail in 
    let random_char = List.nth st.available_letters rand in 
    let st' = update_available_letters true random_char st in 
    let st'' = update_bot_hand (add_letter_to_hand random_char) st' in 
    fill_bot_hand st''

let remove_letter_from_player_hand char st : state = 
  update_player_hand (remove_letter_from_hand char) st

let remove_letter_from_bot_hand char st : state = 
  update_bot_hand (remove_letter_from_hand char) st

let put_on_board x y c st = 
  let letter = letter_of_char c in 
  let tile = st.board.(x).(y) in 
  if tile.status = Set then raise InvalidTilePlacement
  else
    let tile' = {
      tile with 
      status = Filled;
      letter = letter
    } in 
    st.board.(x).(y) <- tile';
    {
      st with
      coords = (x,y)::st.coords
    }

(** [is_row lst] returns whether [lst] contains coordinates that 
    can be placed all in a single row or column. 
    Raises: [InvalidTilePlacement] if [lst] contains coordinates of multiple
    rows, columns, or both. *)
let is_row lst = failwith "Unimplemented"


(** [check_word tlst] is whether the English word formed by the letters of 
    [tlst], in order of reverse-insertion (from left to right). *)
let check_word (tlst : tile list) = 
  let string_of_tile str tile =
    str ^ (tile.letter |> fst |> Char.escaped) in 
  let word = List.fold_left string_of_tile "" tlst in 
  valid_words |> TreeSet.member word

(** [column_word b coords] checks a column of tiles for any word formed 
    by tile placement on [b], then returns the list of words as a 
    [tile tile list]. *)
let column_word b (x,y) =
  let rec column_up x y b acc = 
    if y = 0 then acc
    else if b.(x).(y).status = Empty then acc 
    else column_up x (y-1) b (b.(x).(y)::acc) in
  let rec column_down x y b acc = 
    if y = 14 then acc
    else if b.(x).(y).status = Empty then acc 
    else column_up x (y+1) b (acc @ [b.(x).(y)]) in 
  column_up x y b [] @ (b.(x).(y)::column_down x y b [])

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

(** [score_of_words coords b] is the sum total of the score to be added for a
    given [coords] list in [b]. *)
let score_of_words coords b = 
  if is_row (coords) then 
    let word_list = List.map (column_word b) coords in 
    let words_are_valid = 
      List.fold_left (fun acc tlst -> acc && (check_word tlst)) true word_list 
    in if words_are_valid then 
      List.fold_left (fun acc word -> acc + score_of_word 1 0 word) 0 word_list
    else raise InvalidWords
  else failwith "Unimplemented"

(** [reset_coords st] resets the coordinates list of [st]. *)
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

let update_score st : int = 
  score_of_words (st.coords) (st.board)

(** [confirm_player_turn f st] is the game state with the player's score 
    updated by applying [f]. Should be called by main. *)
let confirm_player_turn f st = 
  {
    st with 
    player_score = f (st)
  } |> set_board

(** [change_score_bot f st] is the game state with the bot's score 
    updated by applying [f]. Should be called by main. *)
let change_score_bot f st = 
  {
    st with 
    bot_score = f (st)
  }

let init_state : state = 
  {
    board = init_board ();
    player_hand = [];
    bot_hand = [];
    coords = [];
    letter_bag = init_bag ();
    available_letters = init_available_letters;
    player_score = 0;
    bot_score = 0;
  }
