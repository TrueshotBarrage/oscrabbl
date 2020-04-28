let rec check_file f =
  match Yojson.Basic.from_file f with
  | exception Sys_error _ -> begin 
      ANSITerminal.(
        print_string [magenta] 
          "Invalid file. Type your json file name without quotes, and make sur\
           e the path to your file is correct.\n"); 
      print_string "> ";
      check_file (read_line ())
    end
  | j -> j

open Adventure
open Command
open State

(** [rand_elt lst] returns a random element from [lst]. If [lst] is empty, 
    it returns a predefined custom string (error message). *)
let rand_elt lst = 
  match lst with
  | [] -> "... nowhere. There's no exits from here. You're screwed. (\"quit\")"
  | _::_ -> lst |> List.length |> Random.int |> List.nth lst

(** [print_items adv st] prints the current room's starting items. *)
let print_items (adv:Adventure.t) (st:State.t) = 
  let rec loop acc = function
    | [] -> ANSITerminal.(print_string [yellow] (acc ^ "\n"))
    | h::t -> loop (acc ^ (Adventure.item_name h) ^ "   ") t
  in loop "Items: " ((st |> current_room_id |> find_entry) st.world_items).items

(** [print_description adv st] prints the current room's description for a 
    given state. *)
let print_description (adv:Adventure.t) (st:State.t) = 
  ANSITerminal.(
    print_string [cyan]
      ((st |> current_room_id |> description adv) ^ "\n")
  ); 
  print_items adv st

let rec print_inventory inv = 
  match inv with
  | [] -> ()
  | h::t -> begin
      ANSITerminal.(print_string [yellow] (h ^ "   "));
      print_inventory t
    end

(** [update_state adv st cmd_str] is a helper function that [continue] calls to 
    update the current state of the game. It handles both good (quit, score, go) 
    and bad ([Empty] or [Malformed] commands) commands and updates to the 
    correct corresponding state. *)
let update_state (adv:Adventure.t) (st:State.t) (cmd_str:string) : t = 
  match parse cmd_str with
  | Quit -> begin 
      ANSITerminal.(print_string [yellow] "See ya later, alligator!\n"); 
      exit 0 
    end
  | exception Empty -> begin 
      ANSITerminal.(print_string [magenta] "... hmm?\n"); 
      st 
    end
  | exception Malformed -> begin 
      let exts = State.exit_list st in
      ANSITerminal.(
        print_string [magenta] 
          ("Uh oh. I didn't catch that. You can use \"go <location>\" to go som\
            ewhere, or type \"quit\" to get outta here.\n");
        print_string [yellow] ("Hint: try \"go " ^ rand_elt exts ^ "\".\n")
      ); 
      st 
    end
  | Score -> begin
      ANSITerminal.(print_string [yellow] (
          "You have " ^ (st |> State.score |> string_of_int) ^ " points.\n"
        ));
      st
    end
  | Inventory -> begin
      ANSITerminal.(
        print_string [yellow] ("Your inventory: "); 
        print_inventory (st |> State.inven);
        print_string [yellow] ("\n")
      );
      st
    end
  | Take item -> begin 
      let result = (String.concat " " item |> take_item) adv st in
      match result with 
      | Legal new_st -> begin
          ANSITerminal.(
            print_string [cyan] (
              "You picked up the " 
              ^ String.concat " " item ^ ".\n"
            );
            print_string [yellow] ("Your inventory: "); 
            print_inventory (new_st |> State.inven);
            print_string [yellow] ("\n")
          );
          new_st
        end
      | Illegal -> begin 
          ANSITerminal.(
            print_string [magenta] "I can't pick that up.\n";
          ); 
          st
        end
    end
  | Drop item -> begin 
      let result = (String.concat " " item |> drop_item) adv st in
      match result with 
      | Legal new_st -> begin
          ANSITerminal.(
            print_string [red] (
              "You dropped the " 
              ^ String.concat " " item ^ ".\n"
            );
            print_string [yellow] ("Your inventory: "); 
            print_inventory (new_st |> State.inven);
            print_string [yellow] ("\n")
          );
          if game_won adv new_st then
            ANSITerminal.(print_string [cyan] (win_msg adv ^ "\n"));
          new_st
        end
      | Illegal -> begin 
          ANSITerminal.(
            print_string [magenta] (
              "I don't see the " 
              ^ String.concat " " item ^ " in your inventory.\n"
            );
          ); 
          st
        end
    end
  | Go exit -> begin 
      let result = (String.concat " " exit |> go) adv st in
      match result with 
      | Legal new_st -> begin
          print_description adv new_st;
          new_st
        end
      | Illegal -> begin 
          let exts = State.exit_list st in
          ANSITerminal.(
            print_string [magenta] "... No. Let's not go there.\n";
            print_string [yellow] ("Hint: try \"go " ^ rand_elt exts ^ "\".\n")
          ); 
          st
        end
    end

(** [continue adv st] recursively keeps the game going by updating the state, 
    printing the description of the room, then reprompting the user. *)
let rec continue (adv:Adventure.t) (st:State.t) : unit = 
  match read_line () with
  | exception End_of_file -> ()
  | command -> begin
      let new_state = update_state adv st command in 
      print_string "> ";
      continue adv new_state
    end

let play_game f =
  let adv = check_file f |> from_json in
  (* print_endline (Yojson.Basic.to_string j); *)
  let state = init_state adv in
  print_description adv state;
  print_string "> ";
  continue adv state

let main () =
  print_string 
    "\nFatal error: exception Failure(\"Parsing error: Expected in\
     t, got null\")\nmake: *** [play] Error 2\n\n\n\n\n\n\n\n...\n\nJ\
     ust kidding haha. Get jebaited";
  ANSITerminal.(
    print_string [red] "\n\nWelcome to the 3110 Text Adventure Game engine.\n"
  );
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
