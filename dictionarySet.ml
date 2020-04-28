open Dictionary

module type ElementSig = sig
  type t
  include Dictionary.KeySig with type t := t
end

module type Set = sig
  module Elt : ElementSig
  type elt = Elt.t
  type t
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

module Make =
  functor (E : ElementSig) -> functor (DM : DictionaryMaker) ->
  struct
    module Elt = E
    type elt = Elt.t

    module Unit = struct
      type t = unit
      let compare x y = EQ
      let format fmt x = ()
    end;;

    (** AF: {k1:v1, k2:v2, ..., kn:vn} represents the set {k1, k2, ..., kn}.
        Since dictionaries can't have duplicate keys, sets made this way will 
        also not include duplicate elements.
        RI: None *)
    module Dict = DM (Elt) (Unit)
    type t = Dict.t

    let rep_ok s = s

    let empty = Dict.empty

    let is_empty s = Dict.is_empty s

    let size s = Dict.size s

    let insert x s = Dict.insert x () s

    let member x s = Dict.member x s

    let remove x s = Dict.remove x s

    let choose s = 
      match Dict.choose s with
      | None -> None
      | Some (k,v) -> Some k

    let fold f init s = 
      let pseudo_f k _ acc = f k acc in
      Dict.fold pseudo_f init s

    let union s1 s2 = fold insert s1 s2

    let intersect s1 s2 = 
      fold (fun x s -> if member x s1 && member x s2 then insert x s else s) 
        empty (union s1 s2)

    let difference s1 s2 =
      fold (fun x s -> if member x s2 then s else insert x s) empty s1

    let to_list s =
      s |> Dict.to_list |> List.map fst

    let format fmt d =
      Dict.format fmt d 
  end
