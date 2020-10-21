open Ast 

module String_type_Pairs = 
  struct 
    type t = string
    let compare = String.compare
  end