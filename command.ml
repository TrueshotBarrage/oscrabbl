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
    | [e] -> let arg = String.lowercase_ascii e in 
      if arg = "confirm" then Confirm
      else if arg = "pass" then Pass
      else if arg = "help" then Help
      else if arg = "quit" then Quit
      else raise Malformed
    | h::t -> let arg = String.lowercase_ascii h in 
      if arg = "put" then Put t
      else if arg = "exchange" then Exchange t
      else raise Malformed
  in str |> String.split_on_char ' ' |> List.map String.trim |> classify
