open Ast;;
open Map;;
open Printf;;

module String_type_Pairs = 
  struct 
    type t = string
    let compare = String.compare
  end;;

module Env = Map.Make(String_type_Pairs);;

type typ =
  | Tint                      (* base type int *)
  | Tbool                     (* base type bool *)
  | Tprod of typ * typ        (* product of two types *)
  | Tarrow of typ list * typ * env  (* function type *)
and env = typ Env.t;;

let typ_env = Env.empty;;

exception TypeError of string ;;

let rec print_type (out: out_channel) (t: typ) = 
  match t with
  | Tbool -> fprintf out "bool \n"
  | Tint -> fprintf out "int \n" 
  | Tprod (t1,t2) -> fprintf out "pair = %a * %a \n" print_type t1 print_type t2 
  | _ -> fprintf out "Not a base type"

let type_unary (u : uop) (e : typ) : typ = 
  match u, e with 
  | Ubnot, Tbool -> Tbool
  | Uineg, Tint -> Tint 
  | Upfst, Tprod (x, y) -> x 
  | Upsnd, Tprod (x, y) -> y
  | _ , _ -> raise (TypeError "Invalid Unop: Type mismatch") ;;

let type_binary (u: bop) (e1: typ) (e2: typ) : typ =
  match u, e1, e2 with 
  | Biadd, Tint, Tint -> Tint
  | Bisub, Tint, Tint -> Tint (* integers: sub *)
  | Bimul, Tint, Tint -> Tint (* integers: mul *)
  | Bidiv, Tint, Tint -> Tint (* integers: div *)
  | Bband, Tbool, Tbool -> Tbool (* booleans: and *)
  | Bcleq, Tint, Tint -> Tbool (* compare:  integers, less or equal *)
  | Bceq, Tint, Tint -> Tbool (* compare:  integers, equal *)
  | _ , _,_ -> raise (TypeError "Invalid Binop: Type mismatch");;

let type_expr (e:expr) (env:env):typ =
  let rec type_aux (e:expr) (env:env):typ =
  match e with
  | Econst(c) -> (match c with
                |Cint(c) -> Tint
                |Cbool(b) -> Tbool)
  | Ename (name) -> Env.find name env
  | Eunary(uop,e1) ->  type_unary uop (type_aux e1 env)
  | Ebinary(bop,e1,e2) -> type_binary bop (type_aux e1 env) (type_aux e2 env)
  | _ -> raise (TypeError "Not done")
  in
  type_aux e env;;