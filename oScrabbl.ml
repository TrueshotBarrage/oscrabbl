type letter = char * int

type modifier = 
  | Nil
  | Word of int
  | Char of int
  | Origin

type status = 
  | Empty
  | Filled
  | Set

type tile = {
  modifier: modifier;
  status: status;
  letter: letter
}

let empty_tile = {
  modifier = Nil;
  status = Empty;
  letter = (' ', 0)
}

let triple_word = {
  modifier = Word 3;
  status = Empty;
  letter = (' ', 0)
} 

let double_word = {
  modifier = Word 2;
  status = Empty;
  letter = (' ', 0)    
}

let triple_letter = {
  modifier = Char 3;
  status = Empty;
  letter = (' ', 0)
}

let double_letter = {
  modifier = Char 2;
  status = Empty;
  letter = (' ', 0)    
}

let origin_letter = {
  modifier = Origin;
  status = Empty;
  letter = (' ', 0)
}

type board = (tile array) array

let bucket : letter list = 
  [
    ('A',1); ('B',3); ('C',3); ('D',2); ('E',1); ('F',4); ('G',2); ('H',4); 
    ('I',1); ('J',8); ('K',5); ('L',1); ('M',3); ('N',1); ('O',1); ('P',3); 
    ('Q',10); ('R',1); ('S',1); ('T',1); ('U',1);('V',4); ('W',4); ('X',8); 
    ('Y',4); ('Z',10); (' ',0)
  ]

let letter_qty  = 
  [
    ('A',9); ('B',2); ('C',2); ('D',4); ('E',12); ('F',2); ('G',3); ('H',2); 
    ('I',9); ('J',1); ('K',1); ('L',4); ('M',2); ('N',6); ('O',8); ('P',2); 
    ('Q',1); ('R',6); ('S',4); ('T',6); ('U',4); ('V',2); ('W',2); ('X',1); 
    ('Y',2); ('Z',1); (' ',2)
  ]

let letter_val (l : char) : int = 
  List.find (fun pair -> fst pair = l) bucket |> snd

module Lexicon = Set.Make(String)

(** [read_line i] reads from the input channel [i] and handles EOF. *)
let read_line i = try Some (input_line i) with End_of_file -> None 

(** [lines_from_files filename lx] reads each line from a text file named 
    [filename] and inserts each line into [lx]. *)
let lines_from_files filename lx = 
  let rec lines_from_files_aux i s = 
    match read_line i with 
    | None -> s
    | Some str -> 
      let str = String.trim str in
      s |> Lexicon.add str |> lines_from_files_aux i
  in lines_from_files_aux (open_in filename) lx

let valid_words = lines_from_files "valid_words.txt" Lexicon.empty