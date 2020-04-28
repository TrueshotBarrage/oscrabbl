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
  (** [test_init_bag name res] constructs a test for 
      [init_bag] and matches its result with [res]. *)
  let test_init_bag (name : string) (res : (char * int) array) : test =
    name >:: (fun _ -> 
        assert_equal 
          res init_bag)

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

  (* let print_dictionary (name : string) (res : unit) : test = 
     name >:: (fun _ ->
        assert_equal () (scrabble_dictionary)) *)

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

  let test_fill_player_hand
      (name : string) 
      (hand : int) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_int
          7 (List.length (fill_player_hand init_state).player_hand))

  let test_fill_bot_hand
      (name : string) 
      (hand : int) : test = 
    name >:: (fun _ -> 
        assert_equal 
          ~printer:string_of_int
          7 (List.length (fill_bot_hand init_state).bot_hand))

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
    test_fill_player_hand "Test random player hand" 7;
    test_fill_bot_hand "Test random bot hand" 7;
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