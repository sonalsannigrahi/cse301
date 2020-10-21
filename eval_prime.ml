open Ast;;

module Stack = Map.Make(String_type_Pairs);;

type stack = Stack

type value = 
          | Val of expr
          (*| Val_closed of (expr* stack) *)

let stack = Stack.empty;;

(* let num_stack = Stack.(m |> add "value3" h);;
Stack.find "value3" num_stack;; *)

exception bad_input of string
exception EvalError of string ;;

let rec eval_unary (op:uop) (e:expr): expr =
  match u, e with 
  | _, Ename(name)-> eval_unary op (Stack.find name stack)
  | _, Econst(c)  -> match c with
                  | Uineg, Cint(x)      -> Vint (~-x)
                  | Ubnot, Cbool(x)     -> Vbool (not x)
  | Upfst, Epair(e1,e2) -> e1
  | Upsnd, Epair(e1,e2) -> e2
  | _, _ -> raise (EvalError "Invalid Unop") ;;


let eval_bop_const (op:bop) (c1:const) (c2:const) = 
  |Cint(num),Cint(num1) ->  match op with
                            | Biadd   -> num + num1
                            | Bisub   -> num - num1
                            | Bimul   -> num * num1
                            | Bidiv   -> num / num1
                            | Bcleq   -> num <= num1
                            | Bceq    -> num < num1
  | Cbool(b),Cbool(b1) ->   match op with 
                            | Bband   -> b && b1
  | something_else -> raise(bad_input "bad input for bop const")

let rec eval_binary (op:bop) (e1:expr) (e2:expr):expr =
  match op,e1,e2 with
  | _, Const(c1), Const(c2) -> eval_bop_const op c1 c2
  |_, Ename (name1),Ename (name2) -> eval_binary op (Stack.find name1 stack) (Stack.find name2 stack)
  | _, _ , Ename(name) -> eval_binary op e1 (Stack.find name stack)
  | _,Ename (name), _  -> eval_binary op (Stack.find name stack) e2

let eval (e:expr) (stack:Map) =
  let rec eval_aux (e:expr) (stack:Map) =
  match e with:
  | Econst(c) -> value(c)
  | Ename(name) -> value(Stack.find name stack)
  | Eunary(uop,e1) -> (match eval_aux e1 stack with 
                        | value e -> Value(eval_unary uop e)
                        | _ -> raise (EvalError "Invalid Unop"))
  | Ebinary(bop,e1,e2) -> (match (eval_aux e1 stack),(eval_aux e2 stack) with
                          | e1,e2 -> Value(eval_binary bop e1 e2) )
                          | _ -> raise (EvalError "Invalid Bop"))

  | Eif(cond,e1,e2) -> (match (eval_aux cond stack) with
                        | value(Cbool(b)) -> if b then (eval_aux e1 stack) else (eval_aux e2 stack))
                        | _ -> raise (EvalError "Invalid condition"))
  | Epair(e1,e2)  ->  (match (eval_aux e1 stack),(eval_aux e2 stack) with
                        | value(Econst(c1)),Value(Econst(c2)) -> value(Epair(c1,c2))
                        | _ -> raise (EvalError "Invalid pair"))
  | Efun(l , expr) ->  value(expr)
  | Eapply(e1 , l) -> 
    match eval_aux e1 stack with 
    | value (Fun (v, e))-> eval e Stack.(stack |> add v (eval_aux e2 env))
    |  _ -> raise (EvalError "Cannot apply non functions")

  | Elet(is_rec,name,e1,e2) -> if is_rec then eval (Eapply (e1,Efun (v, e2))) stack 
                               else raise (EvalError "rec not yet implemented"))


  

                              
                                                      


  