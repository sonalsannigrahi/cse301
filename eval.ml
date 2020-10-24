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

type value = 
  | Vbool of bool 
  | Vint of int 
  | Vpair of (value * value)
  | Vfunc of (arg list  * expr * env)
and env = value Env.t;;

let create_env = Env.empty;;



exception EvalError of string ;;

let rec print_value (out: out_channel) (v: value) = 
  match v with
  | Vbool(b) -> fprintf out "Result is a boolean : %b" b
  | Vint(n) -> fprintf out "Result in an integer : %d" n 
  | Vpair(t1, t2) -> fprintf out "Result is a pair : %a * %a" print_value t1 print_value t2 
  | _ -> fprintf out "Result is a function: %d" 2

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
  | Bisub, Vint x, Vint y -> Vint (x - y) (* integers: sub *)
  | Bimul, Vint x, Vint y -> Vint (x * y) (* integers: mul *)
  | Bidiv, Vint x, Vint y -> Vint (x / y) (* integers: div *)
  | Bband, Vbool b1, Vbool b2 -> Vbool (b1 && b2) (* booleans: and *)
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
  | Efun(l, expr) -> Vfunc (l, expr, env)
  (*TO BE FIXED*)
  | Eapply(e1 , l) -> 
  (match e1, l with
  
  | Ename(name), args_num -> (let func = (Env.find name env) in 
                                match func with
                                |Vfunc(args, e, env_vf) -> 
                                    let env_f = ref env_vf in 
                                    for i=0 to ((List.length args)-1) do 
                                      (match (List.nth args i), (List.nth args_num i) with
                                      |(name,typ) , value -> (env_f := Env.add name (eval_aux value env) !env_f))
                                      done; 
                                      eval_aux e !env_f;
                                |_ -> raise (EvalError "Invalid func"))
  | _ , _ -> raise (EvalError "Invalid input for application"))
    (* function 
    | [], [] -> raise (EvalError "There are no arguments and there are no functions")
    | start_exp :: end_exp, [] -> raise (EvalError "There are no arguments")
    | [], start_args :: end_args -> raise (EvalError "There are too many arguments")
    | [v] -> (match eval_aux e1 env with  
                   | Vfunc (v, e, env)-> eval_aux e (env |> Env.add v (eval_aux e2 env))
                   |  _ -> raise (EvalError "Cannot apply non functions"))  *)

  (* v : list of arguments, l: list of arguments, e: expressions, perform iterations 
  (* Evaluate e1 in env, add v to the env, then evaluate e2 in the non-rec *) *) 
  | Elet(is_rec,v,e1,e2) -> if is_rec then raise (EvalError "recursive")
                            else let u = eval_aux e1 env in let envu = Env.add v u env in eval_aux e2 envu
  in
  eval_aux e env;;
