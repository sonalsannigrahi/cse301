open Map
open Ast

module String_type_Pairs: 
  sig 
    type t
    val compare: string -> string -> int
  end

module Eval:
  sig 
    type value
    type env
    val eval_expr: expr -> env -> value
  end

module Env : Map.S with type key = string