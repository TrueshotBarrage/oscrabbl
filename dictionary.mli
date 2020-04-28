(** Maps from keys to values *)

(** [order] represents the concepts of strictly-less-than, 
    equal-to, and strictly greater-than. *)
type order = LT | EQ | GT

(** A [Comparable] is a value that can be compared.
    The comparison is a total order on the values. *)
module type Comparable = sig

  (** The type of comparable values. *)
  type t

  (** [compare t1 t2] is [LT] if [t1] is less than [t2],
      [EQ] if [t1] is equal to [t2], or [GT] if [t1] is
      greater than [t2]. *)
  val compare : t -> t -> order

end

(** A [Formattable] is a value that can be formatted
    with the toplevel printer. *)
module type Formattable = sig 

  (** The type of formattable values. *)
  type t

  (** [format] is a printing function suitable for use
      with the toplevel's [#install_printer] directive.
      It outputs a textual representation of a value of
      type [t] on the given formatter. *)
  val format : Format.formatter -> t -> unit

end

(** A module that matches [KeySig] is suitable for use as
    the type of keys in a [Dictionary]. Keys must be
    comparable and formattable. *)
module type KeySig = sig
  type t
  include Comparable with type t := t
  include Formattable with type t := t
end

(** A module that matches [ValueSig] is suitable for use as
    the type of values in a [Dictionary]. Values must
    be formattable. *)
module type ValueSig = sig
  type t
  include Formattable with type t := t
end

(** A [Dictionary] maps keys to values. *)
module type Dictionary = sig

  (** [Key] is a module representing the type of keys
      in the dictionary and functions on them. *)
  module Key : KeySig

  (** [Value] is a module representing the type of values
      in the dictionary and functions on them. *)
  module Value : ValueSig

  (** [key] is the type of keys in the dictionary. *)
  type key = Key.t

  (** [value] is the type of values in the dictionary. *)
  type value = Value.t

  (** [t] is the type of dictionaries. *)
  type t

  (** [rep_ok d] returns [d] if [d] satisfies its representation
      invariants. It's unusual for a data abstraction to
      expose this function to its clients, but we do so here
      to ensure that you implement it.
      Raises: [Failure] with an unspecified error message
        if [d] does not satisfy its representation invariants. *)
  val rep_ok : t  -> t

  (** [empty] is the empty dictionary *)
  val empty : t

  (** [is_empty d] is [true] iff [d] is empty. *)
  val is_empty : t -> bool

  (** [size d] is the number of bindings in [d]. *
      [size empty] is [0]. *)
  val size : t -> int

  (** [insert k v d] is [d] with [k] bound to [v]. If [k] was already
      bound, its previous value is replaced with [v]. *)
  val insert : key -> value -> t -> t

  (** [member k d] is [true] iff [k] is bound in [d]. *)
  val member : key -> t -> bool

  (** [find k d] is [Some v] if [k] is bound to [v] in [d]; or
      if [k] is not bound, then it is [None]. *)
  val find : key -> t -> value option

  (** [remove k d] contains all the bindings of [d] except
      a binding for [k].  If [k] is not bound in [d], then
      [remove] returns a dictionary with the same bindings
      as [d]. *)
  val remove : key -> t -> t

  (** [choose d] is [Some (k,v)], where [k] is bound to [v]
      in [d].  It is unspecified which binding of [d] is
      returned.  If [d] is empty, then [choose d] is [None]. *)
  val choose : t -> (key * value) option

  (** [fold f init d] is [f kn vn (f ... (f k1 v1 init) ...)],
      if [d] binds [ki] to [vi].  Bindings are processed
      in order from least to greatest, where [k1] is the
      least key and [kn] is the greatest. *)
  val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  (** [to_list d] is an association list containing the same
      bindings as [d].  The  elements in the list are
      in order from the least key to the greatest. *)
  val to_list : t -> (key * value) list

  (** [format] is a printing function suitable for use
      with the toplevel's [#install_printer] directive.
      It outputs a textual representation of a dictionary
      on the given formatter. *)
  val format : Format.formatter -> t -> unit
end

(** A [DictionaryMaker] is a functor that makes a [Dictionary]
    out of modules representing keys and values. *)
module type DictionaryMaker =
  functor (K : KeySig)
    -> functor (V : ValueSig)
    -> Dictionary with module Key = K and module Value = V