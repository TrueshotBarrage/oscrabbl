open Scrabble

(** [state] is a record that indicates the state of the game. *)
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

(** [init_state] is the initial state of every starting board.
    No letters have been distributed to players at this stage. *)
val init_state : state

(** [init_bag] is the starting bag associative array of letters to 
    their quantities. *)
val init_bag : unit -> (char * int) array

(** [fill_player_hand st] fills the player's hand up to seven letters by 
    taking them out of the bag. *)
val fill_player_hand : state -> state

(** [fill_bot_hand st] fills the bot's hand up to seven letters by 
    taking them out of the bag. *)
val fill_bot_hand : state -> state

(** [remove_letter_from_player_hand c st] removes the letter corresponding to 
    [c] from the player's hand in [st]. *)
val remove_letter_from_player_hand : char -> state -> state

(** [remove_letter_from_bot_hand c st] removes the letter corresponding to 
    [c] from the bot's hand in [st]. *)
val remove_letter_from_bot_hand : char -> state -> state

(** [put_on_bard x y c st] sets the board of [st] with position [(x,y)] with 
    the letter corresponding to [c]. *)
val put_on_board : int -> int -> char -> state -> state

(** [set_board st] sets the tiles corresponding to the coordinates of 
    the board of [st]: status [Filled] -> [Set], modifier [_] -> [Nil]. *)
val set_board : state -> unit

(** [reset_board st] resets the board of any [Filled] letters. *)
val reset_board : state -> unit