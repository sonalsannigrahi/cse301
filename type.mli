open Map
open Ast

type env
type typ 

val typ_env : env

val type_expr: expr -> env -> typ

val print_type: out_channel -> typ -> unit