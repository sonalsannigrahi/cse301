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

type typ =
  | Tint                  
  | Tbool                   
  | Tprod of typ * typ        
  | Tarrow of typ list * typ * (env ref)
and env = typ Env.t;;

let typ_env = ref (Env.empty);;

exception TypeError of string ;;

(*QUESTION: why do we need \n ? it messes up pair
  How do we print functions?*)

let rec find_function_in_expr (name:name) (e:expr) =
  match e with
  | Eunary(uop,expr)  -> find_function_in_expr name expr
  | Eapply(expr,l)  -> find_function_in_expr name expr
  | Ebinary(bop,expr1,expr2) -> (find_function_in_expr name expr1) || (find_function_in_expr name expr2)
  | Epair(expr1,expr2) -> (find_function_in_expr name expr1) || (find_function_in_expr name expr2)
  | Eif(expr1,expr2,expr3)   -> (find_function_in_expr name expr1) || (find_function_in_expr name expr1) || (find_function_in_expr name expr3)
  | Elet(is_rec,name,expr1,expr2) -> if is_rec then (find_function_in_expr name expr1) || (find_function_in_expr name expr2) else raise (TypeError "This is not a recursive function")
  | Ename(name2) -> name2 = name
  | _ -> false

let rec equality_types (t1:typ) (t2:typ) = 
  match t1,t2 with 
  | Tint, Tint -> true
  | Tbool,Tbool -> true
  | Tprod(t11,t12),Tprod(t21,t22) -> (equality_types t11 t21) && (equality_types t12 t22)
  | _ -> false

let rec typ_to_ast (t:typ) = 
  match t with
  | Tint -> Ast.Tint
  | Tbool -> Ast.Tbool
  | Tprod(t1,t2) -> Ast.Tprod((typ_to_ast t1), (typ_to_ast t2))
  | Tarrow(l,t,env) -> (let l_type = ref [] in 
                        for i=0 to ((List.length l)-1) do
                        l_type := !l_type @ [typ_to_ast (List.nth l i)]
                        done;
                        Tarrow(!l_type,(typ_to_ast t)))
let rec ast_to_typ (t:Ast.typ) (env:env ref) = 
  match t with
  | Ast.Tint -> Tint
  | Ast.Tbool -> Tbool
  | Ast.Tprod(t1,t2) -> Tprod((ast_to_typ t1 env), (ast_to_typ t2 env))
  | Ast.Tarrow(l,t) -> (let l_type = ref [] in 
                        for i=0 to ((List.length l)-1) do
                        l_type := !l_type @ [ast_to_typ (List.nth l i) env]
                        done;
                        Tarrow(!l_type,(ast_to_typ t env),env))


let rec print_type (out: out_channel) (t: typ) = 
  match t with
  | Tbool -> fprintf out "bool "
  | Tint -> fprintf out "int " 
  | Tprod (t1,t2) -> fprintf out "(%a * %a)" print_type t1 print_type t2 
  (* | Tarrow(l,output_type,env) -> (for i=0 to ((List.length l)-1) do
                                  print_type out (List.nth l i)
                                  done; print_type out output_type) *)
  |  _ -> raise (TypeError "function not printable")

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

let type_expr (e:expr) (env:env ref):typ =
  let rec type_aux (e:expr) (env:env ref):typ =
  match e with
  | Econst(c) -> (match c with
                |Cint(c) -> Tint
                |Cbool(b) -> Tbool)
  | Ename (name) -> Env.find name !env
  | Eunary(uop,e1) ->  type_unary uop (type_aux e1 env)
  | Ebinary(bop,e1,e2) -> type_binary bop (type_aux e1 env) (type_aux e2 env)
  | Eif(cond,e1,e2) -> (match (type_aux cond env) with
                        | Tbool -> (match (type_aux e1 env),(type_aux e2 env) with 
                                  | Tbool, Tbool -> Tbool
                                  | Tint, Tint   -> Tint
                                  | _ , _ -> raise (TypeError "the then and else block must have the same type"))
                        | _ -> raise (TypeError "Invalid condition"))

  | Epair(e1,e2)  -> Tprod((type_aux e1 env),(type_aux e2 env))

  | Efun(l, expr) -> (let l_type = ref [] in 
                      for i=0 to ((List.length l)-1) do
                      (l_type := !l_type @ [snd(List.nth l i)];
                      env:= (Env.add (fst(List.nth l i)) (ast_to_typ(snd(List.nth l i)) env) !env))
                      done;
                      (ast_to_typ (Tarrow(!l_type,typ_to_ast(type_aux expr env))) env))

  | Elet(is_rec,v,e1,e2) -> (if is_rec then 
                            (match e1 with 
                            | Efun(l,expr) -> (match expr with 
                                  | Eif(cond,expr1,expr2) -> ( 
                                      match (find_function_in_expr v expr1),(find_function_in_expr v expr2 ) with
                                      | false, true -> type_aux expr1 env
                                      | true, false -> type_aux expr2 env
                                      | false, false -> type_aux (Elet(false,v,e1,e2)) env (*If we cannot find the function its not a recursive function*)
                                      | _ , _ -> raise (TypeError "Invalid recursion function")) (* For now*)
                                  | _ -> (if (find_function_in_expr v expr) then raise (TypeError "Invalid recursion function")
                                   else (type_aux (Elet(false,v,e1,e2)) env)))
                            | _ -> raise (TypeError "Not covered 2")) (*If we cannot find the function its not a recursive function*)
                                                        
                      else 
                      let u = type_aux e1 env in
                      let envu =  ref (Env.add v u !env) in
                      (type_aux e2 envu))

  | Eapply(name , l) ->
            (match name with
              | Ename(name) -> (let func = (Env.find name !env) in
                             
                              (match func with
                              | Tarrow(args,output,env_f) -> ( if (List.length args <> List.length l) then raise (TypeError "Incorrect number of arguments") else
                                                            for i=0 to ((List.length l)-1) do
                                                            (assert (equality_types (List.nth args i) (type_aux (List.nth l i) env)))
                                                            done;output)
                              | _ -> raise (TypeError "Not a func")))
                | _ -> raise (TypeError "Not a name") )
  in
  type_aux e env;;