(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Score
  | Inventory
  | Take of object_phrase
  | Drop of object_phrase
  | Quit

exception Empty

exception Malformed

(** [trim_whitespace str_lst] returns the same string list but without any 
    "" empty string elements. *)
let trim_whitespace str_lst = 
  let rec loop lst acc = 
    match lst with
    | [] -> List.rev acc
    | h::t -> begin
        if h <> "" then loop t (h::acc)
        else loop t acc
      end
  in loop str_lst []

let parse str =
  let classify = function
    | [] -> raise Empty
    | [h] -> begin
        if h = "quit" then Quit
        else if h = "score" then Score
        else if h = "inventory" then Inventory
        else raise Malformed
      end
    | h::t -> begin
        if h = "go" then Go t
        else if h = "drop" then Drop t
        else if h = "take" then Take t
        else raise Malformed
      end
  in str |> String.split_on_char ' ' |> trim_whitespace |> classify
