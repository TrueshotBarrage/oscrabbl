open Scrabble

(** [state] is a record that indicates the state of the game. *)
type state = {
  board: board;
  player_hand: letter list;
  bot_hand: letter list;
  letter_bag: letter array;
  available_letters: char list;
  player_score: int;
  bot_score: int;
}

(** [init_state] is the initial state of every starting board.
    No letters have been distributed to players at this stage. *)
val init_state : state

(** [init_bag] is the starting bag associative array of letters to 
    their quantities. *)
val init_bag : (char * int) array

(** [fill_player_hand st] fills the player's hand up to seven letters by 
    taking them out of the bag. *)
val fill_player_hand : state -> state

(** [fill_bot_hand st] fills the bot's hand up to seven letters by 
    taking them out of the bag. *)
val fill_bot_hand : state -> state

(** [remove_letter_from_player_hand st c] removes the letter corresponding to 
    [c] from the player's hand in [st]. *)
val remove_letter_from_player_hand : state -> char -> state

(** [remove_letter_from_bot_hand st c] removes the letter corresponding to 
    [c] from the bot's hand in [st]. *)
val remove_letter_from_bot_hand : state -> char -> state