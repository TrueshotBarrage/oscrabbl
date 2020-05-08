(**
   Parsing of player commands.
*)

(** [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["put A at 3 4"], then the object phrase is 
      [["A"; "at"; "3"; "4"]].
    - If the player command is ["put    A   at 3 4"], then the object phrase is
      again [["A"; "at"; "3"; "4"]].

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Put of object_phrase
  | Confirm
  | Clear
  | Exchange of object_phrase
  | Pass
  | Help
  | Quit

(** [Empty] is raised when an empty command is parsed. *)
exception Empty

(** [Malformed] is raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "put    A   at 3 4"] is [Put ["A"; "at"; "3"; "4"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9), space 
    characters (only ASCII character code 32; not tabs or newlines, etc.), 
    and the wildcard character [*].

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb:
    - is none of: "put", "confirm", "clear", "exchange", "pass", "help", "quit"
    - is "confirm", "clear", "pass", "help", or "quit", and there is a 
      non-empty object phrase
    - is "put" or "exchange" and there is an empty object phrase. *)
val parse : string -> command