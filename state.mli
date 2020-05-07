open Scrabble

exception InvalidTilePlacement
exception InvalidWords
exception SingleLetter

(** [state] is a record that indicates the state of the game. *)
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

(** [index c] is the 0-based index of [c] in the alphabet. 
    If [c] is the space character [' '] with ASCII code 32, return -33. *)
val index : char -> int

(** [index' n] is the reverse operation of [index c]. 
    [index' -33] is the space character [' ']. *)
val index' : int -> char

(** [init_bag] is the starting bag associative array of letters to 
    their quantities. *)
val init_bag : unit -> (char * int) array

(** [update_available_letters removing c st] either: 
    1) takes the letter corresponding to [c] from the letter bag and 
    removes [c] from the list of available letters if the last letter 
    of [c] was taken from the bag, if [removing].
    Requires: The letter of [c] is in the list of available letters. 
    or: 
    2) adds the letter corresponding to [c] to the letter bag and adds 
    [c] to the list of available letters if [c] does not exist in it,
    if not [removing]. *)
val update_available_letters : bool -> char -> state -> state

(** [fill_hand st] fills the player's hand up to seven letters by 
    taking them out of the bag. *)
val fill_hand : state -> state

(** [use_letter c st] removes the letter corresponding to 
    [c] from the current player's hand in [st]. *)
val use_letter : char -> state -> state

(** [put_on_bard (x,y) c st] sets the board of [st] with position [(x,y)] with 
    the letter corresponding to [c]. *)
val put_on_board : string option -> int * int -> char -> state -> state

(** [is_row lst b] returns whether [lst] contains coordinates that can be placed
    all in a single row or column, and whether the placement is valid for [b].
    Raises: [InvalidTilePlacement] if [lst] contains coordinates of multiple
    rows, columns, or both; also raised if [lst] does not form a connected 
    (aka valid) board state if inserted. *)
val is_row : (int * int) list -> board -> bool

(** [score_move st] is the sum total of the score to be added for a
    given [st.coords] list in [st.board], with the scoring word. *)
val score_move : state -> (int * string)

(** [reset_coords st] is the new state [st'] with an empty coordinates list. *)
val reset_coords : state -> state

(** [set_board st] sets the tiles corresponding to the coordinates of 
    the board of [st]: status [Filled] -> [Set], modifier [_] -> [Nil]. *)
val set_board : state -> unit

(** [reset_board st] resets the board of any [Filled] letters. *)
val reset_board : state -> unit

(** [put_everything_back st] puts [Filled] letters on the board back to the 
    hand from which it came, and resets the board accordingly. *)
val put_everything_back : state -> state

(** [set_history pl wd s st] sets the recent history of played words 
    according to which player [pl] played word [wd] for [s] points in [st]. *)
val set_history : string -> string -> int -> state -> state

(** [confirm_turn st] attempts to score the newly filled tiles by the 
    player, and on success, accumulates the points to the player's score. The 
    board is updated with the filled letters to be permanently set, and the
    coordinates of the filled tiles is reset. *)
val confirm_turn : state -> state

(** [pass_turn st] is the new state with the current player's turn passed. *)
val pass_turn : state -> state

(** [init_state ()] is the initial state of every starting board.
    No letters have been distributed to players at this stage. *)
val init_state : unit -> state

(** [result] is either a [Valid] [state], or raises an [exn] if [Invalid]. *)
type result = Valid of state | Invalid of exn

(** [make_move (x,y) c st] attempts to make a placement on the board, checking 
    all the necessary constraints (bag, hand, board, etc.) to see if the move 
    is [Valid] or [Invalid]. *)
val make_move : int * int -> char -> state -> result