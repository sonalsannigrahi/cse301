open Ast;;
open Map;;
open Printf;;
open Printer;;

module String_type_Pairs = 
  struct 
    type t = string
    let compare = String.compare
  end;;

module Env = Map.Make(String_type_Pairs);;

(*Environment: A Map from strings to values
  Value: Defined VBool, Vint, Vpair, and Vfunc which takes a reference to Environment as well so that the env is now mutable*)

type value = 
  | Vbool of bool 
  | Vint of int 
  | Vpair of (value * value)
  | Vfunc of (arg list  * expr * (env ref))
and env = value Env.t;;

let create_env = ref Env.empty;;

exception EvalError of string ;;


let rec print_value (out: out_channel) (v: value) = 
  match v with
  | Vbool(b) -> fprintf out "bool : %b" b
  | Vint(n) -> fprintf out "int : %d" n 
  | Vpair(t1, t2) -> fprintf out "pair : (%a * %a)" print_value t1 print_value t2 
  | _ -> fprintf out "Function is not a base result of evaluation"

let eval_unary (u : uop) (e : value) : value = 
  match u, e with 
  | Ubnot, Vbool b -> Vbool (not b)
  | Uineg, Vint x -> Vint (~-x)
  | Upfst, Vpair (x, y) -> x
  | Upsnd, Vpair (x, y) -> y
  | _ , _ -> raise (EvalError "Invalid Unop") ;;

let eval_binary (u: bop) (e1: value) (e2: value) : value =
  match u, e1, e2 with 
  | Biadd, Vint x, Vint y -> Vint (x + y)
  | Bisub, Vint x, Vint y -> Vint (x - y) 
  | Bimul, Vint x, Vint y -> Vint (x * y) 
  | Bidiv, Vint x, Vint y -> Vint (x / y) 
  | Bband, Vbool b1, Vbool b2 -> Vbool (b1 && b2) 
  | Bcleq, Vint x, Vint y -> Vbool (x <= y) 
  | Bceq, Vint x, Vint y -> Vbool (x = y)
  | _ , _,_ -> raise (EvalError "Invalid Binop");;

let eval_expr (e:expr) (env:env ref):value =
  let rec eval_aux (e:expr) (env:env ref):value =
  match e with
  | Econst(c)          -> (match c with
                        |Cint(c) -> Vint(c)
                        |Cbool(b) -> Vbool(b))
  | Ename (name)       -> Env.find name !env
  | Eunary(uop,e1)     ->  eval_unary uop (eval_aux e1 env)
  | Ebinary(bop,e1,e2) -> eval_binary bop (eval_aux e1 env) (eval_aux e2 env)

  | Eif(cond,e1,e2) -> (match (eval_aux cond env) with
                        | Vbool(b) -> if b then (eval_aux e1 env) else (eval_aux e2 env)
                        | _ -> raise (EvalError "Invalid condition"))
  | Epair(e1,e2)  -> Vpair(eval_aux e1 env,eval_aux e2 env )
  | Efun(l, expr) -> Vfunc (l, expr, env)

  (* In Eapply, we loop over the arguments and add the args to the env in which the function is defined
      and update the env in each iteration. Eval aux is then called with this updated env*)
  | Eapply(e1 , l) -> 

   ( match e1, l with
  | Ename(name), args_num -> (let func = (Env.find name !env) in
                                
                                match func with
                                |Vfunc(args, e, env_vf) -> 
                                    if (List.length args <> List.length args_num) then raise (EvalError "Incorrect number of arguments") else
                                    let env_f = env_vf in 
                                    for i=0 to ((List.length args)-1) do 
                                      (match (List.nth args i), (List.nth args_num i) with
                                      |(name,typ) , value -> (env_f := Env.add name (eval_aux value env) !env_f))
                                      done; 
                                      eval_aux e env_f;
                                |_ -> raise (EvalError "Invalid func"))
  | _ , _ -> raise (EvalError "Invalid input for application"))

  (* In Elet, we handle the non-recursive case by first evaluating in the env for expression 1, 
  then adding the name to this same env and then evaluating expression 2 in this env that has the name for expression 1 
  
  For the recursive case, instead of adding the name to the environment, we store the function itself (which includes the env passed by reference) for later use
  After this, we first evaluate expression 1 in this env, add name: evaluation of expression 1 to the env, then evaluate expression 2*)
  | Elet(is_rec,v,e1,e2) -> if is_rec 
                            then (match eval_aux e1 env with
                            |Vfunc(l, expr, env) -> (env := (Env.add v (Vfunc(l, expr, env)) !env); let u = eval_aux e1 env in
                                                    let envu = ref (Env.add v u !env) in
                                                    eval_aux e2 envu)
                            | _ -> raise (EvalError "Invalid rec"))
                            else let u = eval_aux e1 env in
                            let envu = ref (Env.add v u !env) in
                            eval_aux e2 envu
  in
  eval_aux e env;;
