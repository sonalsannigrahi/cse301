
module Stack = Map.Make(String_type_Pairs);;


type valu = Vbool
          | Vint
          | Vfunc

let stack = Stack.empty;;

(* let num_stack = Stack.(m |> add "value3" h);;
Stack.find "value3" num_stack;; *)

exception bad_input of string

(*Figure out how to do this function*)
let look_up name = 
  match  with

let rec eval_unary (op:uop) (e:expr) =
  match e with
   | Ename(name)                         -> match op with
                                          | Upfst    -> fst (Stack.find name stack)
                                          | Upsnd    -> snd (Stack.find name stack)
                                          | Uineg    ->  - (Stack.find name stack)
                                          | Ubnot    -> not (Stack.find name stack) 

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
  |Ename (name),Econst  (c) -> eval_bop_const op (Stack.find name stack) c
  |Econst  (c), Ename(name) -> eval_bop_const op c (Stack.find name stack)
  | _, _ -> eval
                              
                                                      


  