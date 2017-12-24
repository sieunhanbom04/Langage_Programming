
type ident = string

type unop =
  | Uneg (* -e *)
  | Unot (* not e *)
  | Unstar (*dereference*)
  | Unp (*reference*)
  | Unmutp (*reference mutable*)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)
  | Bassign

type instruction =
  | Inothing
  | Iexpr of expr
  | ImutExAssign of ident * expr
  | ImutStAssign of ident * ident * ((ident * expr) list )
  | IexAssign of ident * expr
  | IstAssign of ident * ident * ((ident * expr) list )
  | Iwhile of expr * block
  | Ireturn of expr
  | ICreturn of ident * expr
  | IreturnNull
  | ICreturnNull of ident
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
  | Ecall of ident * expr list
  | Evector of expr list
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
          | Tstructgeneric of ident * typ
          | Tref of typ
          | Tmut of typ

type struct_argument = {
  name_struct_arg : ident;
  type_struct_arg : typ
}

type argument = {
  name_arg : ident;
  type_arg : typ;
  mut_arg : bool;
}

type decl_fun = {
  name_func : string;
  def_func : argument list;
  return_func : typ;
  body_func : block;
}

type decl_struct = {
  name_struct : string;
  def_struct : struct_argument list;
}

type decl = Decl_fun of decl_fun
            | Decl_struct of decl_struct

type program = decl list
