(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)
(** [exit] represents an exit from a [room] to a different [room]. *)
type exit

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of room identifiers. *)
type room_id = string

(** The type of exit names. *)
type exit_name = string

(** [item] is an item object in the adventure. *)
type item = {
  token: string;
  location: room_id;
  worth: int;
}

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** Raised when an unknown item is encountered. *)
exception UnknownItem of string

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [points a r] is the point value of the room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val points : t -> room_id -> int

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)
(** [list_of_exits] loops similarly to [exits], but returns the list of exits 
    directly, rather than returning the list of the exit names. *)
val list_of_exits : t -> room_id -> exit list

(** [find_exit ext exts] is the exit with name [ext] in [exts]. 
    Raises: [UnknownExit ext] if [ext] is not an exit name in [exts] *)
val find_exit : exit_name -> exit list -> exit

(** [get_exit_room_id ext] returns the exit [ext]'s room id. *)
val get_exit_room_id : exit -> room_id

(** [remove_dups lst] removes any duplicate elements from [lst] and returns 
    the modified list. Returns a "set-like list" of [lst]. Not guaranteed to 
    be in the same order of input. *)
val remove_dups : 'a list -> 'a list

(** [room_starting_items adv room] is the list of starting items that [room] 
    had at its initial state. *)
val room_starting_items : t -> room_id -> item list

(** [item_name item] is the name of the item as a string. *)
val item_name : item -> string

(** [item_loc item] is the location of the item as a string. *)
val item_loc : item -> string

(** [items adv] is the list of items in an adventure [adv]. *)
val items : t -> item list

(** [get_item_from_token itm itm_lst] retrieves the item record [itm] from 
    [itm_lst]. *)
val get_item_from_token : string -> item list -> item

(** [treasure_room adv] is the designated treasure room in [adv]. *)
val treasure_room : t -> room_id

(** [win_msg adv] is the win message for [adv]'s victory condition. *)
val win_msg : t -> string