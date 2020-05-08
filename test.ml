open OUnit2
open Scrabble
open State

(********************************************************************
   Here are some helper functions for testing set-like lists. 
 ********************************************************************)

(** [cmp_unordered_lists lst1 lst2] compares two lists to see whether
    they are "equivalent" lists. That means checking that they contain the 
    same elements, though not necessarily in the same order. *)
let cmp_unordered_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [string_of_tuple f (x,y)] is the string of [x] and string of [y]. *)
let string_of_tuple f (x,y) = "(" ^ f x ^ "," ^ f y ^ ")"

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_letter l] pretty-prints a letter of (char * int) [l]. *)
let pp_letter l = 
  "('" ^ (l |> fst |> Char.escaped) ^ "'," ^ (l |> snd |> string_of_int) ^ ")"

(** [pp_string_opt str_opt] pretty-prints a string option [str_opt]. *)
let pp_string_opt = function
  | None -> "No words found"
  | Some str -> str

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End helper functions.
 ********************************************************************)

(** [ScrabbleTestMaker] is a module to test the game elements. *)
module ScrabbleTestMaker = struct
  (** [test_letter_val name l res] constructs a test for 
      [letter_val l] and matches its result with [res]. *)
  let test_letter_val name l res = 
    name >:: fun _ -> assert_equal ~printer:string_of_int res (letter_val l)

  open Scrabble
  open TreeSet

  (** [test_dictionary_size name res] constructs a test for 
        the size of [valid_words] and matches its result with [res]. *)
  let test_dictionary_size name res = name >:: (
      fun _ -> assert_equal ~printer:string_of_int 
          res (valid_words |> size))

  (** [test_valid_words_member name word res] constructs a test for [member] 
      [word] of the Scrabble Dictionary and matches its result with [res]. *)
  let test_valid_words_member name word res = name >:: (
      fun _ -> assert_equal ~printer:string_of_bool res 
          (valid_words |> member word))

  (** [test_valid_words_choose name res] constructs a test for [choose] 
      of the Scrabble Dictionary and matches its result with [res]. *)
  let test_valid_words_choose name res = name >:: (
      fun _ -> assert_equal ~printer:pp_string_opt res (valid_words |> choose))

  let tests = [
    test_letter_val "Test 'E' => 1" 'E' 1;
    test_letter_val "Test 'X' => 8" 'X' 8;
    test_letter_val "Test ' ' => 0" ' ' 0;
    test_dictionary_size "Test dict size" 279496;
    test_valid_words_member "Test whether 'HELLO' in dict" "HELLO" true;
    test_valid_words_member "Test whether 'POGCHAMP' in dict" "POGCHAMP" false;
    test_valid_words_choose "Test to see if anything in dict" (Some "LAMINAL");
  ]
end

module StateTestMaker = struct
  (** [st0] is the initial state, used as a benchmark for other state tests. *)
  let st0 = init_state ()
  let st0' = game_start ()

  (** [st1] is a state to test general operations, with both row and column 
      placements, as well as valid and invalid tile placements. *)
  let st1 = 
    init_state () |> put_on_board None (7,7) 'A' |> put_on_board None (7,8) 'P'
    |> put_on_board None (7,9) 'P' |> put_on_board None (7,10) 'L' 
    |> put_on_board None (7,11) 'E'
  let st1' = 
    init_state () |> put_on_board None (7,6) 'A' |> put_on_board None (7,7) 'P'
    |> put_on_board None (7,8) 'P' |> put_on_board None (7,9) 'L' 
    |> put_on_board None (7,10) 'E'
  let st1_set = 
    init_state () |> put_on_board None (7,7) 'A' |> put_on_board None (7,8) 'P' 
    |> put_on_board None (7,9) 'P' |> put_on_board None (7,10) 'L' 
    |> put_on_board None (7,11) 'E' |> confirm_turn
  let st1_set_put_back = 
    game_start () |> put_on_board None (7,7) 'A' |> put_on_board None (7,8) 'P' 
    |> put_on_board None (7,9) 'P' |> put_on_board None (7,10) 'L' 
    |> put_on_board None (7,11) 'E' |> confirm_turn
  let st1_add_col = 
    init_state () |> put_on_board None (7,7) 'A' |> put_on_board None (7,8) 'P' 
    |> put_on_board None (7,9) 'P' |> put_on_board None (7,10) 'L' 
    |> put_on_board None (7,11) 'E' |> confirm_turn 
    |> put_on_board None (5,12) 'Y' |> put_on_board None (6,12) 'E'
    |> put_on_board None (7,12) 'S'
  let st1_add_bad_col =
    let st1_aux = 
      init_state () |> put_on_board None (7,7) 'A' 
      |> put_on_board None (7,8) 'P' |> put_on_board None (7,9) 'P' 
      |> put_on_board None (7,10) 'L' |> put_on_board None (7,11) 'E' in 
    set_board st1_aux; 
    st1_aux |> put_on_board None (9,7) 'R' |> put_on_board None (10,7) 'M'
    |> put_on_board None (11,7) 'S'

  (** [st2] is a state to test adding a row to an existing column. *)
  let st2 = 
    init_state () |> put_on_board None (5,7) 'Y' |> put_on_board None (6,7) 'E'
    |> put_on_board None (7,7) 'T'
  let st2_add_row = 
    init_state () |> put_on_board None (5,7) 'Y' |> put_on_board None (6,7) 'E' 
    |> put_on_board None (7,7) 'T' |> confirm_turn
    |> put_on_board None (7,8) 'R' |> put_on_board None (7,9) 'E' 
    |> put_on_board None (7,10) 'E'

  (** [st3] is a state with a wildcard word. *)
  let st3 = 
    init_state () |> put_on_board None (7,7) 'C' 
    |> put_on_board (Some "*") (7,8) 'A' |> put_on_board None (7,9) 'R'

  (** [st4] is a state that tests adding history for various configurations. *)
  let st4 = 
    game_start () |> set_history "" "" 0 |> set_history "" "" 0
    |> set_history "" "" 0 
  let st4_capped = 
    game_start () |> set_history "" "" 0 |> set_history "" "" 0
    |> set_history "" "" 0 |> set_history "" "" 0 
  let st4_filled = 
    game_start () |> set_history "" "" 0 |> set_history "" "" 0
    |> set_history "" "" 0 |> set_history "" "" 0 |> set_history "" "" 0

  (** [st5] is a state to test successfully throwing a [SingleLetter] exception 
      for placing a single tile at the beginning of the game. *)
  let st5 = game_start () |> put_on_board None (7,7) 'H'

  (** [st6] is a state to test whether the bag and the list of available 
      letters are updated correctly. *)
  let st6 = init_state () |> update_available_letters true 'Z'
  let st6_no_b = 
    init_state () |> update_available_letters true 'B'
    |> update_available_letters true 'B'

  (** [test_init_bag name res] constructs a test for 
      [init_bag ()] and matches its result with [res]. *)
  let test_init_bag name res = name >:: fun _ -> assert_equal res (init_bag ())

  (** [test_add_available_letters name c st st'] constructs a test for 
      [update_available_letters false c st] and checks whether its list of 
      available letters matches that of [st']. *)
  let test_add_available_letters name c st st' = name >:: (
      fun _ -> assert_equal ~printer:(pp_list Char.escaped) 
          ~cmp:cmp_unordered_lists st'.available_letters 
          (update_available_letters false c st).available_letters
    )

  (** [exhaust_bag st] continually removes letters from the list of available 
      letters of [st] until no letter remains in the bag. *)
  let rec exhaust_bag st = 
    match st.available_letters with 
    | [] -> st
    | h::t -> update_available_letters true h st |> exhaust_bag

  (** [test_remove_available_letters name c st] constructs a test to see if  
      all elements can be removed correctly, and also that the bag is empty. *)
  let test_remove_available_letters name c st = 
    let st' = exhaust_bag st in name >:: (
        fun _ -> assert_equal true (
            st'.available_letters = [] 
            && Array.for_all (fun (_,qty) -> qty = 0) st'.letter_bag
          ))

  (** [test_fill_both_hands name hand] constructs a test for [fill_hand 
      init_state ()] for both players and matches whether it is fully filled. *)
  let test_fill_both_hands name hand = 
    let gs = game_start () in name >:: (
        fun _ -> assert_equal ~printer:(string_of_tuple string_of_int) (7,7)
            (List.length gs.player_hand, List.length gs.bot_hand)
      )

  (** [test_is_row name coords board res] constructs a test for [is_row coords 
      board] and matches its result with [res]. *)
  let test_is_row name coords board res = name >:: (
      fun _ -> assert_equal ~printer:string_of_bool res (is_row coords board)
    )

  (** [test_is_row_exn coords board] constructs a bad test for [is_row coords 
      board] and checks whether it returns an [InvalidTilePlacement] exn. *)
  let test_is_row_exn name coords board = name >:: (
      fun _ -> assert_raises InvalidTilePlacement (
          fun () -> is_row coords board
        )
    )

  (** [test_score_move name st res] constructs a test for [score_move st] and 
      matches its score result with [res]. *)
  let test_score_move name st res = name >:: (
      fun _ -> assert_equal ~printer:string_of_int res (score_move st |> fst)
    )

  (** [test_reset_coords name st] constructs a test for [reset_coords st] 
      and matches its result with [[]]. *)
  let test_reset_coords name st = name >:: (
      fun _ -> assert_equal 
          ~printer:(string_of_int |> string_of_tuple |> pp_list) 
          [] ((reset_coords st).coords)
    )

  (** [check_board_not_filled b] is whether all the tiles in [b] are not 
      [Filled]. *)
  let check_board_not_filled b = 
    Array.for_all (
      fun t_arr -> Array.for_all (fun tile -> tile.status <> Filled) t_arr
    ) b 

  (** [test_reset_board name st is_not_filled] checks if none of the tiles in 
      [st]'s board are [Filled], by comparing with [is_not_filled]. *)
  let test_reset_board name st is_not_filled = name >:: (
      fun _ -> assert_equal ~printer:string_of_bool
          is_not_filled (check_board_not_filled st.board)
    )

  (** [test_put_everything_back name st exp_hand_size] checks whether 
      [put_everything_back st] returns the board to its unfilled state, as well 
      as putting both players' hand sizes to be equal to [exp_hand_size]. *)
  let test_put_everything_back name st exp_hand_size = 
    let st_aux = put_everything_back st in name >:: (
        fun _ -> assert_equal ~printer:string_of_bool true 
            (check_board_not_filled st_aux.board 
             && List.length st_aux.player_hand = exp_hand_size 
             && List.length st_aux.bot_hand = exp_hand_size)
      )

  (** [test_set_history name pl wd s st history] checks whether the history 
      was updated properly through a black box, and checks the length to be 4 if
      not simply prepended to the array. *)
  let test_set_history name pl wd s st = name >:: (
      fun _ -> assert_equal true (
          (pl,wd,s)::st.history = (set_history pl wd s st).history
          || List.length st.history = 4
        ))

  (** [test_confirm_turn_exn1 st] constructs a bad test for [confirm_turn st] 
      and checks whether it returns an [InvalidTilePlacement] exn. *)
  let test_confirm_turn_exn1 name st = name >:: (
      fun _ -> assert_raises InvalidTilePlacement (
          fun () -> confirm_turn st
        )
    )

  (** [test_confirm_turn_exn2 st] constructs a bad test for [confirm_turn st] 
      and checks whether it returns an [InvalidTilePlacement] exn. *)
  let test_confirm_turn_exn2 name st = name >:: (
      fun _ -> assert_raises SingleLetter (
          fun () -> confirm_turn st
        )
    )

  (** [test_pass_turn name st] constructs a test for [pass_turn st] and matches 
      checks whether the turn was changed. *)
  let test_pass_turn name st = name >:: (
      fun _ -> assert_equal ~printer:string_of_bool st.player_turn
          (not (pass_turn st).player_turn)
    )

  (** [test_game_start name] constructs a test for [game_start ()] and checks 
      whether both players' hands are filled, a key distinction from 
      [init_state ()]. *)
  let test_game_start name = 
    let gs = game_start () in name >:: (
        fun _ -> assert_equal ~printer:(
            fun (sz1,sz2) -> 
              "Player hand: " ^ string_of_int sz1 
              ^ "; Bot hand: " ^ string_of_int sz2
          ) (7,7) (List.length gs.player_hand, List.length gs.bot_hand)
      )

  (** [tests] represent the test suite to be run for the functor. *)
  let tests = [
    test_init_bag "Test initialization of letter bag" (Array.of_list [
        ('A',9); ('B',2); ('C',2); ('D',4); ('E',12); ('F',2); ('G',3); ('H',2); 
        ('I',9); ('J',1); ('K',1); ('L',4); ('M',2); ('N',6); ('O',8); ('P',2); 
        ('Q',1); ('R',6); ('S',4); ('T',6); ('U',4); ('V',2); ('W',2); ('X',1); 
        ('Y',2); ('Z',1); (' ',2)
      ]);
    test_add_available_letters 
      "Test not adding a letter to the list in the initial state" 'A' 
      (init_state ()) st0;
    test_add_available_letters
      "Test adding a letter to a state with a letter previously removed" 'Z' 
      st6 st0;
    test_add_available_letters 
      "Test adding a letter to a state that has no effect" 'B' st6_no_b st0;
    test_fill_both_hands "Test random player hand" 7;
    test_is_row "Test a valid column placement" [(5,12); (6,12); (7,12)] 
      st1_add_col.board false;
    test_is_row "Test a valid row placement" [(7,8); (7,9); (7,10)] 
      st2_add_row.board true;
    test_is_row_exn "Test an invalid column placement" [(9,7); (10,7); (11,7)] 
      st1_add_bad_col.board;
    test_score_move "Test scoring a word APPLE" st1' 9;
    test_score_move "Test scoring a word APPLE with doubled E" st1 10;
    test_score_move "Test scoring a word YET" st2 6;
    test_score_move "Test scoring a word TREE by only placing REE"
      st2_add_row 4;
    test_score_move "Test scoring a word with a wildcard C*R" st3 4;
    test_reset_coords "Test resetting coordinates of no tiles" st0;
    test_reset_coords "Test resetting coordinates of placed tiles" st1_add_col;
    test_reset_coords "Test resetting coordinates of other tiles" st2_add_row;
    test_reset_board "Test whether initial state board is unfilled" st0 true;
    test_reset_board "Test whether an updated state board is filled" st1 false;
    test_reset_board "Test whether a board after being set is unfilled" st1_set 
      true;
    test_reset_board "Test whether a set board filled again is filled" 
      st1_add_col false;
    test_put_everything_back "Test doing nothing in the initial state" st0 0;
    test_put_everything_back 
      "Test putting back on a set (confirmed) board state" st1_set_put_back 7;
    test_set_history "Test putting record in an empty history" "Player" "DOG" 
      5 st0;
    test_set_history "Test putting record in a full history" "Bot" "GOD" 
      5 st4;
    test_set_history "Test putting record in a full history" "Bot" "GOD" 
      5 st4_capped;
    test_set_history "Test putting record in a full history" "Bot" "GOD" 
      5 st4_filled;
    test_confirm_turn_exn1 "Test a bad confirmation on empty board state" st0;
    test_confirm_turn_exn2 
      "Test a bad confirmation on board state with single letter" st5;
    test_pass_turn "Test random turn change" st0;
    test_pass_turn "Test random turn change again" (pass_turn st0);
    test_pass_turn "Test random turn change on an ongoing game state" st1;
    test_pass_turn "Test random turn change on an ongoing game state again" 
      (pass_turn st1);
    test_game_start "Test starting the game";
  ]
end

(** [scrabble_tests] is the list of tests generated from the 
    [ScrabbleTestMaker] functor. *)
let scrabble_tests = ScrabbleTestMaker.tests

(** [state_tests] is the list of tests generated from the [StateTestMaker] 
    functor. *)
let state_tests = StateTestMaker.tests

(** [test_suite] is used by OUnit to run tests. *)
let test_suite =
  "Test suite for the project" >::: List.flatten [
    scrabble_tests;
    state_tests;
  ]

(* Runs the tests *)
let _ = run_test_tt_main test_suite