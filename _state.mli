(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(** [room_to_item] is a mapping from a room in adventure to all its items. *)
type room_to_item = {
  room_id: Adventure.room_id;
  items: Adventure.item list;
}

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing the game state. *)
type t = {
  curr_room: Adventure.room_id;
  exts: Adventure.exit_name list;
  vstd_rooms: Adventure.room_id list;
  score: int;
  inventory: string list;
  world_items: room_to_item list;
}

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Adventure.t -> t

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_room_id : t -> string

(** [visited st] is a set-like list of the room identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)
val visited : t -> string list

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)
(** [exit_list st] is the list of exits of [st]'s current room. *)
val exit_list : t -> Adventure.exit_name list

(** [score st] is the score so far in the game state [st]. *)
val score : t -> int

(** [inven st] is the player's inventory in game state [st]. *)
val inven : t -> string list

(** [add_to_rti item room room_to_item_list] updates the entry with id [room] 
    in [room_to_item_list] with [item] added onto the [room] record. *)
val add_to_rti : Adventure.item -> Adventure.room_id 
  -> room_to_item list -> room_to_item list

(** [del_from_rti item room room_to_item_list] updates the entry with id [room] 
    in [room_to_item_list] with [item] removed from the [room] record. *)
val del_from_rti : Adventure.item -> Adventure.room_id 
  -> room_to_item list -> room_to_item list

(** [take_item token adv st] puts the item into the player's inventory, while 
    removing it from the player's current room. *)
val take_item : string -> Adventure.t -> t -> result

(** [drop_item token adv st] drops the item from the player's inventory, while 
    adding it to the player's current room. *)
val drop_item : string -> Adventure.t -> t -> result

(** [find_entry rm] finds the entry in the mapping with room_id [rm]. *)
val find_entry : Adventure.room_id -> room_to_item list -> room_to_item

(** [game_won st] checks whether the victory condition has been reached. *)
val game_won : Adventure.t -> t -> bool