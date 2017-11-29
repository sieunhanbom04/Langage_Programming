
type ident = string

type unop =
  | Uneg (* -e *)
  | Unot (* not e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type instruction =
  | IExpr of expr
  | IWhile of expr * block
  | IReturn of expr
  | IReturnNull
  (*à complet*)
and expr =
  | Econst of int
  | Ebool of bool
  | Evar of string
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Estruct of expr * ident
  | Elength of expr
  | Eindex of expr * expr
  | Eprint of string
  (*à complet*)
and block =
  | CFullBlock of instruction list * expr
  | CBlock of instruction list

type typ = Tident of ident
           | Tnull
           (* à complet*)

type struct_argument = {
  name : ident;
  types : typ
}

type argument = {
  name : ident;
  types : typ;
  mut : bool;
}

type decl_fun = {
  name : string;
  defs : argument list;
  return : typ;
  body : block;
}

type decl_struct = {
  name : string;
  defs : struct_argument list;
}

type decl = Decl_fun of decl_fun
            | Decl_struct of decl_struct

type program = decl list
