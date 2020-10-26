open Map
open Ast

type value

type env



val eval_expr: expr -> env ref -> value

val print_value: out_channel -> value -> unit 
