type object_phrase = string list

type command = 
  | Put of object_phrase
  | Confirm
  | Exchange of object_phrase
  | Pass
  | Help
  | Quit

exception Empty
exception Malformed

let parse str =
  let classify = function
    | [] -> raise Empty
    | [arg] -> 
      if arg = "confirm" then Confirm
      else if arg = "pass" then Pass
      else if arg = "help" then Help
      else if arg = "quit" then Quit
      else raise Malformed
    | arg::obj_lst ->
      if arg = "put" then Put obj_lst
      else if arg = "exchange" then Exchange obj_lst
      else raise Malformed
  in str |> String.lowercase_ascii |> String.split_on_char ' ' 
     |> List.map String.trim |> classify
