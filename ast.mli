open Location

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
  | Inothing of location
  | Iexpr of expr * location
  | IexAssign of ident * expr * bool * location
  | IstAssign of ident * ident * ((ident * expr) list ) * bool * location
  | Iwhile of expr * block * location
  | Ireturn of expr * location
  | ICreturn of ident * expr * location
  | IreturnNull of location
  | ICreturnNull of ident * location
  | Icond of condition * location
and expr =
  | Econst of int * location
  | Ebool of bool * location
  | Evar of ident * location
  | Ebinop of binop * expr * expr * location
  | Eunop of unop * expr * location
  | Estruct of expr * ident * location
  | Elength of expr * location
  | Eindex of expr * expr * location
  | Eprint of string * location
  | Ecall of ident * expr list * location
  | Evector of expr list * location
  | Eblock of block * location
and block =
  | CFullBlock of instruction list * expr * location
  | CBlock of instruction list * location
and condition =
  | Cif of expr * block * (block option) * location
  | CnestedIf of expr * block * condition * location

type typ = Tnull
          | Tint
          | Tbool
          | Tstruct of ident
          | Tstructgeneric of ident * typ
          | Tref of typ
          | Tmut of typ

type struct_argument = {
  name_struct_arg : ident;
  type_struct_arg : typ;
  loc_struct_arg : location;
}

type argument = {
  name_arg : ident;
  type_arg : typ;
  mut_arg : bool;
  loc_arg: location;
}

type decl_fun = {
  name_func : string;
  def_func : argument list;
  return_func : typ;
  body_func : block;
  loc_func: location;
}

type decl_struct = {
  name_struct : string;
  def_struct : struct_argument list;
  loc_struct: location;
}

type decl = Decl_fun of decl_fun
            | Decl_struct of decl_struct

type program = decl list
