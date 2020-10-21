open Ast
open Printf

let p_name (name:name) =
  printf "%s" name;;

let p_const (cst:const) = 
  match cst with
  |Cint(num) -> printf "%d" num
  |Cbool(b)  -> printf "%b" b

let p_uop (op:uop) = 
  match op with
  | Uineg -> printf "-"
  | Ubnot -> printf "~"
  | Upfst -> printf "fst"
  | Upsnd -> printf "snd"

let p_bop (op:bop) = 
  match op with 
  | Biadd -> printf " + "
  | Bisub -> printf " - "
  | Bimul -> printf " * "
  | Bidiv -> printf " / "
  | Bband -> printf " & "
  | Bcleq -> printf " <= "
  | Bceq  -> printf " == "

let rec p_typ (t:typ) = 
  match t with
  | Tint                        -> printf "int"
  | Tbool                       -> printf "bool"
  | Tprod(typ1,typ2)            -> (p_typ typ1;printf " * ";p_typ typ2)  (*Not sure*)
  | Tarrow(l,typ2)              -> (for i=0 to ((List.length l)-1) do
                                      (p_typ (List.nth l i); printf " -> ")
                                    done;
                                    p_typ typ2;)


let p_arg (a:arg) = (printf "(";p_name (fst a); printf " : "; p_typ (snd a);printf ")")

let rec p_expr (e:expr) = 
  match e with
  | Econst  (c)                         -> p_const c
  | Ename   (name)                      -> p_name name
  | Eunary  (uop,expr)                  -> (p_uop uop;p_expr expr)
  | Ebinary (bop,expr1, expr2)          -> (p_expr expr1; p_bop bop; p_expr expr2)
  | Eif     (expr1 , expr2 , expr3)     -> (printf " if ";p_expr expr1;printf " then ";p_expr expr2;printf "\n"; printf " else "; p_expr expr3)
  | Epair   (expr1 , expr2)             -> (printf "(";p_expr expr1;printf ",";p_expr expr2; printf ")")
  | Elet    (b , name , expr1 , expr2)  -> if b then (printf "let rec ";p_name name;printf " = ";p_expr expr1;printf "in \n"; p_expr expr2)
                                           else (printf "let ";p_name name;printf " = ";p_expr expr1;printf " in \n"; p_expr expr2)
  | Efun    (l , expr)                  -> (printf "fun "; 
                                            for i=0 to ((List.length l)-1) do (p_arg (List.nth l i);printf " ") done;
                                            printf " -> ";
                                            p_expr expr)
  | Eapply  (expr1 , l)                 -> (p_expr expr1; printf " "; 
                                            for i=0 to ((List.length l)-1) do (p_expr (List.nth l i);printf " ") done;)

