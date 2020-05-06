open OUnit2
open Scrabble
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
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

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_letter l] pretty-prints a letter of (char * int) [l]. *)
let pp_letter l = 
  "('" ^ (l |> fst |> Char.escaped) ^ "'," ^ (l |> snd |> string_of_int) ^ ")"

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

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_unordered_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_unordered_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]
(********************************************************************
   End helper functions.
 ********************************************************************)

module ScrabbleTestMaker = struct
  (** [test_letter_val name l res] constructs a test for 
      [letter_val l] and matches its result with [res]. *)
  let test_letter_val (name : string) (l : char) (res : int) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_int 
          res (letter_val l))

  let tests = [
    test_letter_val "Test 'E' => 1" 'E' 1;
    test_letter_val "Test ' ' => 0" ' ' 0;
  ]
end

module StateTestMaker = struct
  let st1 = 
    init_state () |> put_on_board (7,7) 'A' |> put_on_board (7,8) 'P'
    |> put_on_board (7,9) 'P' |> put_on_board (7,10) 'L' 
    |> put_on_board (7,11) 'E'
  let st1_add_col = 
    set_board st1; 
    st1 |> put_on_board (5,12) 'Y' |> put_on_board (6,12) 'E'
    |> put_on_board (7,12) 'S'
  let st1_add_bad_col =
    st1 |> put_on_board (9,7) 'R' |> put_on_board (10,7) 'M'
    |> put_on_board (11,7) 'S'

  let st2 = 
    init_state () |> put_on_board (5,7) 'Y' |> put_on_board (6,7) 'E'
    |> put_on_board (7,7) 'E' |> put_on_board (8,7) 'T'
  let st2_add_row = 
    set_board st2;
    st2 |> put_on_board (8,8) 'R' |> put_on_board (8,9) 'E'
    |> put_on_board (8,10) 'E'

  (** [test_init_bag name res] constructs a test for 
      [init_bag] and matches its result with [res]. *)
  let test_init_bag (name : string) (res : (char * int) array) : test =
    name >:: (fun _ -> 
        assert_equal 
          res (init_bag ()))

  open Scrabble
  open TreeSet
  let scrabble_dictionary = valid_words

  (** [test_dictionary_size name res] constructs a test for 
        the size of [valid_words] and matches its result with [res]. *)
  let test_dictionary_size (name : string) (res : int) : test =
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_int
          res (scrabble_dictionary |> size))

  (** [test_valid_words_member name word res] constructs a test for [member] 
      [word] of the Scrabble Dictionary and matches its result with [res]. *)
  let test_valid_words_member
      (name : string) 
      (word : elt)
      (res : bool) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_bool
          res (scrabble_dictionary |> member word))

  (** [pp_string_opt str] pretty-prints a string option [str]. *)
  let pp_string_opt (str : string option) = 
    match str with
    | None -> "No words found"
    | Some str -> str

  (** [test_valid_words_choose name res] constructs a test for [choose] 
      of the Scrabble Dictionary and matches its result with [res]. *)
  let test_valid_words_choose
      (name : string) 
      (res : elt option) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:pp_string_opt
          res (scrabble_dictionary |> choose))

  (** [test_fill_both_hands name hand] constructs a test for [fill_hand 
      init_state ()] for both players and matches whether it is fully filled. *)
  let test_fill_both_hands
      (name : string) 
      (hand : int) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_int
          7 (List.length (
              init_state () |> fill_hand |> pass_turn |> fill_hand
            ).player_hand))

  (** [test_is_row name coords board res] constructs a test for [is_row coords 
      board] and matches its result with [res]. *)
  let test_is_row
      (name : string) 
      (coords : (int * int) list)
      (board : board) 
      (res : bool) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_bool
          res (is_row coords board))

  let test_is_row_exn
      (name : string) 
      (coords : (int * int) list)
      (board : board) : test = 
    name >:: (fun _ -> 
        assert_raises (InvalidTilePlacement) (fun () -> is_row coords board))

  let tests = [
    test_init_bag "Test initialization of letter bag" 
      (Array.of_list 
         [('A',9); ('B',2); ('C',2); ('D',4); ('E',12); ('F',2); ('G',3); ('H',2); 
          ('I',9); ('J',1); ('K',1); ('L',4); ('M',2); ('N',6); ('O',8); ('P',2); 
          ('Q',1); ('R',6); ('S',4); ('T',6); ('U',4); ('V',2); ('W',2); ('X',1); 
          ('Y',2); ('Z',1); (' ',2)]);
    test_dictionary_size "Test dict size" 279496;
    test_valid_words_member "Test whether 'HELLO' in dict" 
      "HELLO" true;
    test_valid_words_member "Test whether 'POGCHAMP' in dict" 
      "POGCHAMP" false;
    test_valid_words_choose "Test to see if anything in dict" (Some "LAMINAL");
    test_fill_both_hands "Test random player hand" 7;
    test_is_row "Test a valid column placement" [(5,12); (6,12); (7,12)] 
      st1_add_col.board false;
    test_is_row "Test a valid row placement" [(8,8); (8,9); (8,10)] 
      st2_add_row.board true;
    test_is_row_exn "Test an invalid column placement" [(9,7); (10,7); (11,7)] 
      st1_add_bad_col.board;
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