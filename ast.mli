
type ident = string

type unop =
  | Uneg (* -e *)
  | Unot (* not e *)
  | Unstar
  | Unp
  | Unmutp

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type instruction =
  | Inothing
  | Iexpr of expr
  | ImutExAssign of ident * expr
  | ImutStAssign of ident * ident * ((ident * expr) list )
  | IexAssign of ident * expr
  | IstAssign of ident * ident * ((ident * expr) list )
  | Iwhile of expr * block
  | Ireturn of expr
  | IreturnNull
  | Icond of condition
and expr =
  | Econst of int
  | Ebool of bool
  | Evar of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Estruct of expr * ident
  | Elength of expr
  | Eindex of expr * expr
  | Eprint of string
  | Evector of ident * (expr list)
  | Eblock of block
and block =
  | CFullBlock of instruction list * expr
  | CBlock of instruction list
and condition =
  | Cif of expr * block * block
  | CnestedIf of expr * block * condition

type typ = Tnull
          | Tint
          | Tbool
          | Tstruct of ident
          | Tvec of typ
          | Tref of typ
          | Tmut of typ

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
