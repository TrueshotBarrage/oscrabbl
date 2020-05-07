open OUnit2
open Scrabble
open State

(********************************************************************
   Here are some helper functions for testing set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_unordered_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

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
  let st0 = init_state ()
  let st1 = 
    init_state () |> put_on_board None (7,7) 'A' |> put_on_board None (7,8) 'P'
    |> put_on_board None (7,9) 'P' |> put_on_board None (7,10) 'L' 
    |> put_on_board None (7,11) 'E'
  let st1' = 
    init_state () |> put_on_board None (7,6) 'A' |> put_on_board None (7,7) 'P'
    |> put_on_board None (7,8) 'P' |> put_on_board None (7,9) 'L' 
    |> put_on_board None (7,10) 'E'
  let st1_add_col = 
    let st1_aux = 
      init_state () |> put_on_board None (7,7) 'A' 
      |> put_on_board None (7,8) 'P' |> put_on_board None (7,9) 'P' 
      |> put_on_board None (7,10) 'L' |> put_on_board None (7,11) 'E' in 
    set_board st1_aux; 
    st1_aux |> put_on_board None (5,12) 'Y' |> put_on_board None (6,12) 'E'
    |> put_on_board None (7,12) 'S'
  let st1_add_bad_col =
    let st1_aux = 
      init_state () |> put_on_board None (7,7) 'A' 
      |> put_on_board None (7,8) 'P' |> put_on_board None (7,9) 'P' 
      |> put_on_board None (7,10) 'L' |> put_on_board None (7,11) 'E' in 
    set_board st1_aux; 
    st1_aux |> put_on_board None (9,7) 'R' |> put_on_board None (10,7) 'M'
    |> put_on_board None (11,7) 'S'

  let st2 = 
    init_state () |> put_on_board None (5,7) 'Y' |> put_on_board None (6,7) 'E'
    |> put_on_board None (7,7) 'T'
  let st2_add_row = 
    init_state () |> put_on_board None (5,7) 'Y' |> put_on_board None (6,7) 'E' 
    |> put_on_board None (7,7) 'T' |> confirm_turn
    |> put_on_board None (7,8) 'R' |> put_on_board None (7,9) 'E' 
    |> put_on_board None (7,10) 'E'

  (** [test_init_bag name res] constructs a test for 
      [init_bag ()] and matches its result with [res]. *)
  let test_init_bag name res = name >:: fun _ -> assert_equal res (init_bag ())

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

  (** [test_pass_turn name st] constructs a test for [pass_turn st] and matches 
      checks whether the turn was changed. *)
  let test_pass_turn name st = name >:: (
      fun _ -> assert_equal ~printer:string_of_bool st.player_turn
          (not (pass_turn st).player_turn)
    )

  let tests = [
    test_init_bag "Test initialization of letter bag" 
      (Array.of_list 
         [('A',9); ('B',2); ('C',2); ('D',4); ('E',12); ('F',2); ('G',3); ('H',2); 
          ('I',9); ('J',1); ('K',1); ('L',4); ('M',2); ('N',6); ('O',8); ('P',2); 
          ('Q',1); ('R',6); ('S',4); ('T',6); ('U',4); ('V',2); ('W',2); ('X',1); 
          ('Y',2); ('Z',1); (' ',2)]);
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
    test_reset_coords "Test resetting coordinates of no tiles" st0;
    test_reset_coords "Test resetting coordinates of placed tiles" st1_add_col;
    test_reset_coords "Test resetting coordinates of other tiles" st2_add_row;
    test_pass_turn "Test random turn change" st0;
    test_pass_turn "Test random turn change again" (pass_turn st0);
  ]
end

module ScrabbleTests = ScrabbleTestMaker
let scrabble_tests = ScrabbleTests.tests

module StateTests = StateTestMaker
let state_tests = StateTests.tests

let test_suite =
  "Test suite for the project" >::: List.flatten [
    scrabble_tests;
    state_tests;
  ]

let _ = run_test_tt_main test_suite