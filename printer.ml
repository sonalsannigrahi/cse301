open Ast
open Printf

let p_name (outch: out_channel) (name:name) =
  fprintf outch "%s" name;;

let p_const (outch: out_channel) (cst:const) = 
  match cst with
  |Cint(num) -> fprintf outch "%d" num
  |Cbool(b)  -> fprintf outch "%b" b

let p_uop (outch: out_channel) (op:uop) = 
  match op with
  | Uineg -> fprintf outch "-"
  | Ubnot -> fprintf outch "~"
  | Upfst -> fprintf outch "fst"
  | Upsnd -> fprintf outch "snd"

let p_bop (outch: out_channel) (op:bop) = 
  match op with 
  | Biadd -> fprintf outch " + "
  | Bisub -> fprintf outch " - "
  | Bimul -> fprintf outch " * "
  | Bidiv -> fprintf outch " / "
  | Bband -> fprintf outch " & "
  | Bcleq -> fprintf outch " <= "
  | Bceq  -> fprintf outch " == "

let rec p_typ (outch: out_channel) (t:typ) = 
  match t with
  | Tint                        -> fprintf outch "int"
  | Tbool                       -> fprintf outch "bool"
  | Tprod(typ1,typ2)            -> fprintf outch "%a * %a" p_typ typ1 p_typ typ2  (*Not sure*)
  | Tarrow(l,typ2)              -> (for i=0 to ((List.length l)-1) do
                                      (p_typ outch (List.nth l i); fprintf outch " -> ")
                                    done;
                                    p_typ outch typ2;)


let p_arg (outch: out_channel) (a:arg) = fprintf outch "( %a : %a )" p_name (fst a) p_typ (snd a)

let rec p_expr (outch: out_channel) (e:expr) = 
  match e with
  | Econst  (c)                         -> p_const outch c
  | Ename   (name )                     -> p_name outch name
  | Eunary  (uop,expr)                  -> (p_uop outch uop;p_expr outch expr)
  | Ebinary (bop,expr1, expr2)          -> (p_expr outch expr1; p_bop outch bop; p_expr outch expr2)
  | Eif     (expr1 , expr2 , expr3)     -> (fprintf outch " if %a then %a else %a" p_expr expr1 p_expr expr2 p_expr expr3)
  | Epair   (expr1 , expr2)             -> (fprintf outch "( %a , %a )" p_expr expr1 p_expr expr2)
  | Elet    (b , name , expr1 , expr2)  -> if b then (fprintf outch "let rec %a = %a in %a" p_name name p_expr expr1 p_expr expr2)
                                           else (fprintf outch "let %a = %a in %a" p_name name p_expr expr1 p_expr expr2)
  | Efun    (l , expr)                  -> (fprintf outch "fun "; 
                                            for i=0 to ((List.length l)-1) do (p_arg outch (List.nth l i);fprintf outch " ") done;
                                            fprintf outch " -> ";
                                            p_expr outch expr)
  | Eapply  (expr1 , l)                 -> (p_expr outch expr1; fprintf outch " "; 
                                            for i=0 to ((List.length l)-1) do (p_expr outch (List.nth l i);fprintf outch " ") done;)

