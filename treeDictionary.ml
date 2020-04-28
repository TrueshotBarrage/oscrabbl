open Dictionary

(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association 
    list [lst] as a dictionary.  The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively.  The
    [fmt] argument is where to put the formatted output. *)
let format_assoc_list format_key format_val fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a -> %a; "
                format_key k format_val v) lst;
  Format.fprintf fmt "]"

module Make
  = functor (K : KeySig) -> functor (V : ValueSig) ->
  struct
    module Key = K
    module Value = V
    type key = K.t
    type value = V.t

    (** AF: The red-black tree [(k1,v1), (k2,v2), ..., (kn,vn)] represents the 
        dictionary {k1:v1, k2:v2, ..., kn:vn}.
        RI: 
        1. For all [Node]s [n] in [t], all keys in the left subtree of [n] 
        return [LT] when compared to the key of [n], and all keys in the 
        right subtree of [n] return [GT] when compared to the key of [n].
        2. For every path from the root down to any [Leaf], the path has the 
        same number of [Black] [Node]s.
        3. No [Red] [Node] has a [Red] child.
    *)
    type color = Red | Black
    type t = 
      | Leaf 
      | Node of color * (key * value) * t * t

    (* Set debug to true to use rep_ok *)
    let debug = false
    let rep_ok d =
      if debug then
        (** [bst_rep_ok d] is whether the RI for the BST is satisfied for [d]. 
        *)
        let rec bst_rep_ok = function
          | Leaf -> true
          | Node (_,(k,v),lt,rt) -> begin
              let check_order =
                match lt,rt with
                | Node (_,(k1,_),_,_), Node (_,(k2,_),_,_) -> 
                  (Key.compare k1 k = LT) && (Key.compare k2 k = GT)
                | Leaf, Node (_,(k2,_),_,_) -> (Key.compare k2 k = GT)
                | Node (_,(k1,_),_,_), Leaf -> (Key.compare k1 k = LT)
                | _ -> true
              in check_order && (bst_rep_ok lt) && (bst_rep_ok rt)
            end
        in
        (** [global_rbt_rep_ok d] is whether the global RI for the red-black 
            tree (that all paths from the root down to any [Leaf] have the same 
            number of [Black] [Node]s) is satisfied for [d]. *)
        let global_rbt_rep_ok d = 
          let rec count_black_nodes = function
            | Leaf -> 0
            | Node (c,_,lt,rt) -> 
              let l_black_nodes = count_black_nodes lt in 
              let r_black_nodes = count_black_nodes rt in 
              if l_black_nodes = r_black_nodes 
              && l_black_nodes >= 0 && r_black_nodes >= 0 then 
                if c = Black then 1 + r_black_nodes else r_black_nodes
              else -1
          in count_black_nodes d >= 0
        in
        (** [not_red t] checks whether [t] is not colored [Red]. *)
        let not_red = function
          | Leaf -> true
          | Node (c,_,_,_) -> (c <> Red) in 
        (** [local_rbt_rep_ok d] is whether the local RI for the red-black 
            tree (that no [Red] [Node] has a [Red] child) is satisfied for [d].
        *)
        let rec local_rbt_rep_ok = function
          | Leaf -> true
          | Node (c,_,lt,rt) -> 
            if c = Red then 
              not_red lt && not_red rt 
              && local_rbt_rep_ok lt && local_rbt_rep_ok rt
            else local_rbt_rep_ok lt && local_rbt_rep_ok rt
        in
        if (bst_rep_ok d && global_rbt_rep_ok d && local_rbt_rep_ok d) then d 
        else failwith "Does not keep the RI true!"
      else d

    let empty = Leaf

    let is_empty d = d = Leaf

    let rec size d = match d with
      | Leaf -> 0
      | Node (_,_,lt,rt) -> 1 + size lt + size rt

    let insert k v d =
      let d = rep_ok d in
      let balance = function
        | Node(Black, z, Node(Red, y, Node(Red, x, a, b), c), d)
        | Node(Black, z, Node(Red, x, a, Node(Red, y, b, c)), d)
        | Node(Black, x, a, Node(Red, z, Node(Red, y, b, c), d))
        | Node(Black, x, a, Node(Red, y, b, Node(Red, z, c, d)))
          -> Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        | t -> t
      in
      let rec ins k v = function
        | Leaf -> Node (Red,(k,v),Leaf,Leaf)
        | Node (c,(k1,v1),lt,rt)->
          if Key.compare k k1 = LT then balance (Node(c,(k1,v1),ins k v lt,rt))
          else if Key.compare k k1 = GT 
          then balance (Node(c,(k1,v1),lt,ins k v rt))
          else Node (c,(k,v),lt,rt) in
      match ins k v d with
      | Node (_,n,lt,rt) -> rep_ok (Node (Black,n,lt,rt))
      | Leaf -> failwith "You can't have just a Leaf after inserting!"

    let remove k d =
      failwith "Unimplemented"

    let rec find k d = 
      match d with
      | Leaf -> None
      | Node (_,(k1,v),lt,rt) -> 
        begin 
          match Key.compare k k1 with
          | LT -> find k lt
          | EQ -> Some v
          | GT -> find k rt
        end

    let member k d =
      match find k d with 
      | Some _ -> true
      | None -> false

    let choose d =
      match d with
      | Leaf -> None
      | Node (_,x,_,_) -> Some x

    let rec to_list d =
      match d with
      | Leaf -> []
      | Node (_,x,lt,rt) -> to_list lt @ [x] @ to_list rt

    let rec fold f acc d =
      match d with
      | Leaf -> acc
      | Node (_,(k,v),lt,rt) -> fold f (fold f (f k v acc) lt) rt

    let format fmt d =
      format_assoc_list Key.format Value.format fmt (to_list d)
  end
