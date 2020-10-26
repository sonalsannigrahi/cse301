open Map
open Ast

type env
type typ 

val typ_env : env ref

val type_expr: expr -> env ref -> typ

val print_type: out_channel -> typ -> unit