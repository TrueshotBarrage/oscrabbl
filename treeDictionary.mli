(** Dictionaries implemented as red-black trees. *)

open Dictionary

(** [MakeListDictionary] makes a [Dictionary] implemented
    with red-black trees.  *)
module Make : DictionaryMaker
