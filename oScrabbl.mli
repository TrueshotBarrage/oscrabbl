(** 
   All the game elements and data structures that are independent of game state.
*)

(** [letter] is the type of letters in the game, with a character and a score 
    value to represent the letter. *)
type letter = char * int

(** [modifier] is the score modifier of the tile, as well as any additional 
    effects needed. [Origin] denotes the center tile of the board. *)
type modifier = 
  | Nil
  | Word of int
  | Char of int
  | Origin

(** [status] is [Empty] when the tile is not occupied, [Filled] when a letter 
    occupies the tile but is not confirmed yet, and [Set] once it is both 
    occupied and set in stone. *)
type status = 
  | Empty
  | Filled
  | Set

(** [tile] is a tile on the board. *)
type tile = {
  modifier: modifier;
  status: status;
  letter: letter;
}

(** [board] is the OScrabbl board. *)
type board = (tile array) array

(** [empty_tile] is the default tile. *)
val empty_tile : tile

(** [triple_word] is the tile that triples the word score. *)
val triple_word : tile

(** [double_word] is the tile that doubles the word score. *)
val double_word : tile

(** [triple_letter] is the tile that triples the letter score. *)
val triple_letter : tile

(** [double_letter] is the tile that doubles the letter score. *)
val double_letter : tile

(** [origin_letter] is the tile where every game must start. *)
val origin_letter : tile

(** [bucket] is the letters and  their scores. *)
val bucket : letter list

(** [letter_qty] is a list that contains the number of letters in the game. *)
val letter_qty : (char * int) list

(** [letter_val l] returns the corresponding point value of letter [l].
    Requires: [l] is a valid letter in OScrabbl. *)
val letter_val : char -> int

(** The module representing a string lookup table, organized as a [Set] from 
    the standard library. (Only [mem], [choose], and [cardinal] 
    are exposed, since we don't need anything else for lookups.) *)
module Lexicon : sig 
  (** [elt] is the type representing the lexicon. *)
  type elt = String.t

  (** [t] is the type of the words in the lexicon, namely [string]. *)
  type t 

  (** [mem wd lx] tests whether [wd] belongs to the lexicon [lx].  *)
  val mem : elt -> t -> bool

  (** [choose lx] is some single element of [lx]. Which element is chosen is
      unspecified, but equal elements will be chosen for equal sets. 
      Raises: [Not_found] if the lexicon is empty. *)
  val choose : t -> elt

  (** [cardinal lx] is the number of words in the lexicon [lx]. *)
  val cardinal : t -> int
end

(** [valid_words] is a [Lexicon] containing all the valid words in OScrabbl. *)
val valid_words : Lexicon.t