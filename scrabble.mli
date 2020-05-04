(** [letter] is the type of letters in the game *)
type letter = char * int

(** [modifier] is the modifier of the tile *)
type modifier = 
  | Nil
  | Word of int
  | Char of int
  | Origin

(** [status] is the status of the tile *)
type status = 
  | Empty
  | Filled
  | Set

(** [tile] is a tile on the board *)
type tile = {
  modifier: modifier;
  status: status;
  letter: letter;
}

(** [board] is the board. *)
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

(**[letter_qty] is a list that contains the number of letters in the game. *)
val letter_qty : (char * int) list

(** letter_val [l] returns the corresponding point value of letter [l].
    Requires: [l] is a valid letter in Scrabble. *)
val letter_val : char -> int

(** [TreeSet] is a sig for the dictionary format. *)
module TreeSet : sig 
  type t
  type elt = string
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

(** [valid_words] is a TreeSet containing all the valid words. *)
val valid_words : TreeSet.t