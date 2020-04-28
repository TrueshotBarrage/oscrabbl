(* Note: You may introduce new code anywhere in this file. *) 

type room_to_item = {
  room_id: Adventure.room_id;
  items: Adventure.item list;
}

type t = {
  curr_room: Adventure.room_id;
  exts: Adventure.exit_name list;
  vstd_rooms: Adventure.room_id list;
  score: int;
  inventory: string list;
  world_items: room_to_item list;
}

let add_to_rti item room room_to_item_list = 
  let rec loop acc (rm:Adventure.room_id) rti_lst = 
    match rti_lst with
    | [] -> acc
    | h::t -> begin
        if h.room_id = rm then 
          let entry = {
            room_id = h.room_id;
            items = h.items @ [item];
          } 
          in loop (entry::acc) rm t
        else loop (h::acc) rm t
      end
  in loop [] room (List.rev room_to_item_list)

let del_from_rti item room room_to_item_list = 
  let rec loop acc (rm:Adventure.room_id) rti_lst = 
    match rti_lst with
    | [] -> acc
    | h::t -> begin
        if h.room_id = rm then 
          let entry = {
            room_id = h.room_id;
            items = List.filter (
                fun x -> Adventure.(item_name x <> item_name item)
              ) h.items;
          } 
          in loop (entry::acc) rm t
        else loop (h::acc) rm t
      end
  in loop [] room (List.rev room_to_item_list)

(** [init_items adv] is the initial list of items when the adventure starts. *)
let init_items (adv:Adventure.t) : room_to_item list = 
  let rec rti_lst acc lst : room_to_item list =
    match lst with
    | [] -> acc
    | h::t -> rti_lst ({
        room_id = h;
        items = [];
      }::acc) t
  in 
  let rec loop acc itms = 
    match itms with 
    | [] -> acc
    | h::t when 
        acc |> 
        List.exists (fun x -> x.room_id = (Adventure.item_loc h)) |> 
        not ->
      loop ({
          room_id = Adventure.item_loc h;
          items = [h];
        }::acc) t
    | h::t -> loop (add_to_rti h (Adventure.item_loc h) acc) t
  in loop (rti_lst [] (Adventure.room_ids adv)) (Adventure.items adv)

let init_state (adv:Adventure.t) : t = 
  let current_room = Adventure.start_room adv in {
    curr_room = current_room;
    exts = Adventure.exits adv current_room;
    vstd_rooms = [current_room];
    score = 0;
    inventory = [];
    world_items = init_items adv;
  }

let current_room_id st = st.curr_room

let visited st = st.vstd_rooms

type result = Legal of t | Illegal

(** [update_state ex adv st] looks at [st] and returns the updated state. 
    This is a subfunction from [go], so the context is a successful [Go] cmd.
    Should not fail because the functions calling this function should 
    guarantee a correct input. *)
let update_state ex adv st : t =
  let new_room = Adventure.next_room adv st.curr_room ex in
  let (vstd, sc) = 
    if List.mem new_room st.vstd_rooms then (st.vstd_rooms, st.score)
    else (new_room::st.vstd_rooms, st.score + Adventure.points adv new_room) 
  in {
    curr_room = new_room;
    exts = Adventure.exits adv new_room;
    vstd_rooms = vstd;
    score = sc;
    inventory = st.inventory;
    world_items = st.world_items;
  }

let go (ex:Adventure.exit_name) (adv:Adventure.t) (st:t) : result =
  let exts = current_room_id st |> Adventure.exits adv in
  if List.mem ex exts then 
    let st' = update_state ex adv st in Legal (st')
  else Illegal

let rec find_entry rm = function
  | [] -> { room_id = ""; items = [] }
  | h::t -> begin
      if h.room_id = rm then h 
      else find_entry rm t
    end

let take_item token adv st : result = 
  let entry = find_entry st.curr_room st.world_items in
  let item_exists = 
    adv |> Adventure.items |> List.map Adventure.item_name |> List.mem token in
  if item_exists then 
    let item = adv |> Adventure.items |> Adventure.get_item_from_token token in
    if List.mem item entry.items then
      let world_items' = 
        st.world_items |> del_from_rti item st.curr_room
      in
      let st' = 
        let score_deduction = 
          if st.curr_room = Adventure.treasure_room adv then item.worth 
          else 0
        in {
          curr_room = st.curr_room;
          exts = st.exts;
          vstd_rooms = st.vstd_rooms;
          score = st.score - score_deduction;
          inventory = st.inventory @ [token];
          world_items = world_items';
        } in
      Legal (st') 
    else Illegal
  else Illegal

let drop_item token adv st : result = 
  let entry = find_entry st.curr_room st.world_items in
  let item_exists = 
    adv |> Adventure.items |> List.map Adventure.item_name |> List.mem token in
  if item_exists then
    let item = adv |> Adventure.items |> Adventure.get_item_from_token token in
    if not (List.mem item entry.items) then
      let world_items' = 
        st.world_items |> add_to_rti item st.curr_room
      in
      let st' = 
        let score_addition = 
          if st.curr_room = Adventure.treasure_room adv then item.worth
          else 0
        in
        {
          curr_room = st.curr_room;
          exts = st.exts;
          vstd_rooms = st.vstd_rooms;
          score = st.score + score_addition;
          inventory = List.filter (fun itm -> itm <> token) st.inventory;
          world_items = world_items';
        } in
      Legal (st')
    else Illegal
  else Illegal

let exit_list st = st.exts

let score st = st.score

let inven st = st.inventory

(** [cmp_itm_lsts lst1 lst2] compares two item lists [lst1] and [lst2] by 
    sorting and reordering the list of the item names of those two lists, then 
    checking if each and every item is equal. *)
let cmp_itm_lsts (lst1:Adventure.item list) lst2 : bool = 
  let item_names1 = List.map Adventure.item_name lst1 in
  let item_names2 = List.map Adventure.item_name lst2 in
  Adventure.(remove_dups item_names1 = remove_dups item_names2)

let game_won adv st = 
  let treasure_room_items = 
    ((adv |> Adventure.treasure_room |> find_entry) st.world_items).items in
  let adventure_items = (adv |> Adventure.items) in
  cmp_itm_lsts treasure_room_items adventure_items