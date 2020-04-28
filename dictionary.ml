type order = LT | EQ | GT

module type Comparable = sig
  type t
  val compare : t -> t -> order
end

module type Formattable = sig 
  type t
  val format : Format.formatter -> t -> unit
end

module type KeySig = sig
  type t
  include Comparable with type t := t
  include Formattable with type t := t
end

module type ValueSig = sig
  type t
  include Formattable with type t := t
end

module type Dictionary = sig
  module Key : KeySig
  module Value : ValueSig
  type key = Key.t
  type value = Value.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : key -> value -> t -> t
  val member : key -> t -> bool
  val find : key -> t -> value option
  val remove : key -> t -> t
  val choose : t -> (key * value) option
  val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> (key * value) list
  val format : Format.formatter -> t -> unit
end

module type DictionaryMaker =
  functor (K : KeySig)
    -> functor (V : ValueSig)
    -> Dictionary with module Key = K and module Value = V
