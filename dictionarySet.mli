(** Sets implemented as dictionaries. *)

open Dictionary

(** The type of elements. *)
module type ElementSig = sig
  type t
  include Dictionary.KeySig with type t := t
end

(** A [Set] contains elements, which must be comparable. *)
module type Set = sig

  (** [Elt] is a module representing the type of elements
      in the set and functions on them. *)
  module Elt : ElementSig

  (** [elt] is the type of elements in the set. *)
  type elt = Elt.t

  (** [t] is the type of sets. *)
  type t

  (** [rep_ok s] returns [s] if [s] satisfies its representation
      invariants.  It's unusual for a data abstraction to
      expose this function to its clients, but we do so here
      to ensure that you implement it.
      Raises: [Failure] with an unspecified error message
        if [s] does not satisfy its representation invariants. *)
  val rep_ok : t  -> t

  (** [empty] is the empty set. *)
  val empty : t

  (** [is_empty s] is [true] iff [s] is empty. *)
  val is_empty : t -> bool

  (** [size s] is the number of elements in [s]. *
      [size empty] is [0]. *)
  val size : t -> int

  (** [insert x s] is a set containing all the elements of
      [s] as well as element [x]. *)
  val insert : elt -> t -> t

  (** [member x s] is [true] iff [x] is an element of [s]. *)
  val member : elt -> t -> bool

  (** [remove x s] contains all the elements of [s] except
      [x].  If [x] is not an element of [s], then
      [remove] returns a set with the same elements as [s]. *)
  val remove : elt -> t -> t

  (** [union] is set union, that is, [union s1 s2] contains
      exactly those elements that are elements of [s1]
      or elements of [s2]. *)
  val union : t -> t -> t

  (** [intersect] is set intersection, that is, [intersect s1 s2]
      contains exactly those elements that are elements of [s1]
      and elements of [s2]. *)
  val intersect : t -> t -> t

  (** [difference] is set difference, that is, [difference s1 s2]
      contains exactly those elements that are elements of [s1]
      but not elements of [s2]. *)
  val difference : t -> t -> t

  (** [choose s] is [Some x], where [x] is an unspecified
      element of [s].  If [s] is empty, then [choose s] is [None]. *)
  val choose : t -> elt option

  (** [fold f init s] is [f xn (f ... (f x1 init) ...)],
      if [s] contains [x1]..[xn].  Elements are processed
      in order from least to greatest, where [x1] is the
      least element and [xn] is the greatest. *)
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  (** [to_list s] is a list containing the same
      elements as [s].  The order of elements in the list is
      in order from the least set element to the greatest. *)
  val to_list : t -> elt list

  (** [format] is a printing function suitable for use
      with the toplevel's [#install_printer] directive.
      It outputs a textual representation of a set
      on the given formatter. *)
  val format : Format.formatter -> t -> unit

end

(** [Make] implements a set as a dictionary.
    The keys of the dictionary represent the elements of the set.  
    The values of the dictionary are irrelevant. *)
module Make :   
  functor (E : ElementSig)
    -> functor (DM : DictionaryMaker)
    -> Set with module Elt = E

