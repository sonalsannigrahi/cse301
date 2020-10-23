
module env = Map.Make(String_type_Pairs);;


type valu = Vbool
          | Vint
          | Vfunc

let env = env.empty;;

(* let num_env = env.(m |> add "value3" h);;
env.find "value3" num_env;; *)

exception bad_input of string

(*Figure out how to do this function*)
let look_up name = 
  match  with

let rec eval_unary (op:uop) (e:expr) =
  match e with
   | Ename(name)                         -> match op with
                                          | Upfst    -> fst (env.find name env)
                                          | Upsnd    -> snd (env.find name env)
                                          | Uineg    ->  - (env.find name env)
                                          | Ubnot    -> not (env.find name env) 

  | Econst  (c)                         -> match c with
                                          |Cint(num) -> match op with
                                                        |Uineg    ->  -c
                                                        |other_op ->  (bad_input "We cannot do this operation with numbers")
                                          |Cbool(b)  -> match op with 
                                                        |Ubnot    -> not c
                                                        |other_op -> raise (bad_input "We cannot do this operation with booleans")

  | Epair   (e1,e2)                     -> match op with
                                          | Upfst    -> e1
                                          | Upsnd    -> e2
                                          | other_op -> raise (bad_input "We cannot do this operation with pairs")
  | Eunary  (uop,expr)                  -> match op with
                                          | Upfst    -> fst eval_unary(uop,expr)
                                          | Upsnd    -> snd eval_unary(uop,expr)
                                          | Uineg    ->  - eval_unary(uop,expr)
                                          | Ubnot    -> not eval_unary(uop,expr)
  | Ebinary (bop,expr1, expr2)          -> match op with
                                          | Upfst    -> fst eval_binary(bop,expr1,expr2)
                                          | Upsnd    -> snd eval_binary(bop,expr1,expr2)
                                          | Uineg    ->  - eval_binary(bop,expr1,expr2)
                                          | Ubnot    -> not eval_binary(bop,expr1,expr2)
  
  | Other_expression                    -> raise(bad_input "Illegal operation")


let eval_bop_const (op:bop) (c1:const) (c2:const) = 
  |Cint(num),Cint(num1) ->  match op with
                                                        | Biadd   -> num + num1
                                                        | Bisub   -> num - num1
                                                        | Bimul   -> num * num1
                                                        | Bidiv   -> num / num1
                                                        | Bcleq   -> num <= num1
                                                        | Bceq    -> num < num1
                              | Cbool(b),Cbool(b1) -> match op with 
                                                        | Bband   -> b && b1
  | something_else -> raise(bad_input "bad input for bop const")

let rec eval_binary (op:bop) (e1:expr) (e2:expr) =
  match e1,e2 with
  |Econst  (c),Econst  (c1) -> eval_bop_const op c c1
  |Ename (name),Econst  (c) -> eval_bop_const op (env.find name env) c
  |Econst  (c), Ename(name) -> eval_bop_const op c (env.find name env)
  | _, _ -> eval
                              
                                                      


  