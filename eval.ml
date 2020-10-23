open Ast;;
module String_type_Pairs = 
  struct 
    type t = string
    let compare = String.compare
  end;;
module Env = Map.Make(String_type_Pairs);;
type value = 
  | Vbool of bool 
  | Vint of int 
  | Vpair of (value * value)
  | Vfunc of arg list  * value

type env = value Env.t;;



let stack = Env.empty;;

(* let num_env = env.(m |> add "value3" h);;
env.find "value3" num_env;; *)

exception EvalError of string ;;

(* let rec eval_unary (op:uop) (e:expr): value =
  match u, e with 
  | _, Econst(c)  -> match c with
                  | Uineg, Vint(x)      -> Vint (~-x)
                  | Ubnot, Vbool(x)     -> Vbool (not x)
  | Upfst, Epair(e1,e2) -> e1
  | Upsnd, Epair(e1,e2) -> e2
  | _, _ -> raise (EvalError "Invalid Unop") ;; *)

  let eval_unary (u : uop) (e : value) : value = 
    match u, e with 
    | Ubnot, Vbool b -> Vbool (not b)
    | Uineg, Vint x -> Vint (~-x)
    | Upfst, Vpair (x, y) -> x (*not sure*)
    | Upsnd, Vpair (x, y) -> y
    | _, _ -> raise (EvalError "Invalid Unop") ;;


(* let eval_bop_const (op:bop) (c1:const) (c2:const):value = 
  match c1,c2 with 
  |Cint(num),Cint(num1) ->  match op with
                            | Biadd   -> Vint(num + num1)
                            | Bisub   -> Vint(num - num1)
                            | Bimul   -> Vint(num * num1)
                            | Bidiv   -> Vint(num / num1)
                            | Bcleq   -> Vbool(num <= num1)
                            | Bceq    -> Vbool(num < num1)
  | Cbool(b),Cbool(b1) ->   match op with 
                            | Bband   -> Vbool(b && b1)
  | something_else -> raise(bad_input "bad input for bop const");;

let rec eval_binary (op:bop) (e1:expr) (e2:expr):expr =
  match op,e1,e2 with
  | _, Const(c1), Const(c2) -> eval_bop_const op c1 c2
  | _ , _, _ -> raise (EvalError "Invalid Binop");; *)


let eval_binary (u: bop) (e1: value) (e2: value) : value =
  match u, e1, e2 with 
  | Biadd, Vint x, Vint y -> Vint (x + y)
  | Bisub, Vint x, Vint y -> Vint (x - y) (* integers: sub      *)
  | Bimul, Vint x, Vint y -> Vint (x * y) (* integers: mul      *)
  | Bidiv, Vint x, Vint y -> Vint (x / y) (* integers: div      *)
  | Bband, Vbool b1, Vbool b2 -> Vbool (b1 && b2) (* booleans: and      *)
  | Bcleq, Vint x, Vint y -> Vbool (x <= y) (* compare:  integers, less or equal *)
  | Bceq, Vint x, Vint y -> Vbool (x = y)  (* compare:  integers, equal *)
  | _ , _,_ -> raise (EvalError "Invalid Binop");;

let eval_expr (e:expr) (env:env):value =
  let rec eval_aux (e:expr) (env:env):value =
  match e with
  | Econst(c) -> (match c with
                |Cint(c) -> Vint(c)
                |Cbool(b) -> Vbool(b))
  | Ename (name) -> Env.find name env
  | Eunary(uop,e1) ->  eval_unary uop (eval_aux e1 env)
  | Ebinary(bop,e1,e2) -> eval_binary bop (eval_aux e1 env) (eval_aux e2 env)

  | Eif(cond,e1,e2) -> (match (eval_aux cond env) with
                        | Vbool(b) -> if b then (eval_aux e1 env) else (eval_aux e2 env)
                        | _ -> raise (EvalError "Invalid condition"))
  | Epair(e1,e2)  -> Vpair(eval_aux e1 env,eval_aux e2 env )
  | Efun(l , expr) -> Vint(0)
  (* | Eapply(e1 , l) -> 
    match eval_aux e1 env with 
    | Vfunc ( (v, e))-> eval_aux e env.(env |> add v (eval_aux e2 env))
    |  _ -> raise (EvalError "Cannot apply non functions")

  | Elet(is_rec,name,e1,e2) -> if is_rec then eval (Eapply (e1,Efun (v, e2))) env 
                               else raise (EvalError "rec not yet implemented") *)
  | _ -> raise (EvalError "No")


  in
  eval_aux e env;;




  

                              
                                                      


  