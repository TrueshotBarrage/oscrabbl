(* Note: You may introduce new code anywhere in this file. *) 

type room_id = string
type exit_name = string
exception UnknownRoom of room_id
exception UnknownExit of exit_name
exception UnknownItem of string

type exit = {
  name: exit_name;
  room_id: room_id;
}

(** [room] represents a room in the adventure. *)
type room = {
  id: room_id;
  description: string;
  points: int;
  exits: exit list;
}

type item = {
  token: string;
  location: room_id;
  worth: int;
}

type t = {
  rooms: room list; 
  start_room: room_id;
  items: item list;
  treasure_room: room_id;
  win: string;
}

open Yojson.Basic.Util

(** [exit_of_json j] is an [exit] in "OCaml-friendly" format from our JSON 
    represented format for an exit. *)
let exit_of_json j = {
  name = j |> member "name" |> to_string;
  room_id = j |> member "room id" |> to_string;
}

(** [room_of_json j] is a [room] in "OCaml-friendly" format from our JSON 
    adventure-rendered room. *)
let room_of_json j = {
  id = j |> member "id" |> to_string;
  description = j |> member "description" |> to_string;
  points = j |> member "points" |> to_int;
  exits = j |> member "exits" |> to_list |> List.map exit_of_json;
}

(** [item_of_json j] is an [item] in "OCaml-friendly" format from our JSON 
    adventure. *)
let item_of_json j = {
  token = j |> member "token" |> to_string;
  location = j |> member "location" |> to_string;
  worth = j |> member "worth" |> to_int;
}

(** [items_of_json j] is an [item list] in "OCaml-friendly" format from our 
    JSON adventure. *)
let items_of_json j = j |> member "items" |> to_list |> List.map item_of_json

(** [t_of_json j] is the adventure in "OCaml-friendly" format from our JSON 
    adventure. *)
let t_of_json j = {
  rooms = j |> member "rooms" |> to_list |> List.map room_of_json;
  start_room = j |> member "start room" |> to_string;
  items = items_of_json j;
  treasure_room = j |> member "treasure room" |> to_string;
  win = j |> member "win" |> to_string;
}

let from_json json = 
  try t_of_json json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let start_room adv = adv.start_room

let room_ids adv = 
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (h.id::acc) t
  in loop [] adv.rooms

(** [find_room rooms rm_id] finds the room whose id matches [rm_id] in [rooms]. 
    Raises: [UnknownRoom rm_id] if [rm_id] is not a room id in [rooms] *)
let rec find_room (rooms:room list) (rm_id:room_id) : room = 
  match rooms with
  | [] -> raise (UnknownRoom rm_id)
  | h::t -> begin 
      if h.id = rm_id then h 
      else find_room t rm_id
    end

let description adv room =
  (find_room adv.rooms room).description

let points adv room = 
  (find_room adv.rooms room).points

let item_name item = item.token

let item_loc item = item.location

let items adv = adv.items

let rec get_item_from_token itm = function
  | [] -> raise (UnknownItem itm)
  | h::t -> if h.token = itm then h else get_item_from_token itm t

let room_starting_items adv room = 
  let rec loop acc = function
    | [] -> acc
    | h::t -> begin
        if h.location = room then loop (h::acc) t
        else loop acc t
      end
  in loop [] adv.items

let exits adv room = 
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (h.name::acc) t
  in loop [] (find_room adv.rooms room).exits

let list_of_exits adv room : exit list = 
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (h::acc) t
  in loop [] (find_room adv.rooms room).exits

let rec find_exit (ext:exit_name) (exts:exit list) : exit = 
  match exts with
  | [] -> raise (UnknownExit ext)
  | h::t -> begin 
      if h.name = ext then h
      else find_exit ext t
    end

let next_room adv room ex : room_id =
  (room |> list_of_exits adv |> find_exit ex).room_id

let get_exit_room_id (ext:exit) : room_id = ext.room_id

let remove_dups lst = 
  let rec loop acc = function
    | [] -> acc
    | [h] -> h::acc
    | h1::(h2::t2 as t) -> begin
        if h1 <> h2 then loop (h1::acc) t
        else loop acc t
      end
  in loop [] (List.sort_uniq compare lst)

let next_rooms adv room =
  room |> list_of_exits adv |> List.map get_exit_room_id |> remove_dups

let treasure_room adv = adv.treasure_room

let win_msg adv = adv.win