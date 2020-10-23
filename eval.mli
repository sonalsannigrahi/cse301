open Map
open Ast

type value

type env

val create_env : env

val eval_expr: expr -> env -> value

val print_value: out_channel -> value -> unit 
