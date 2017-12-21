open Ast

type pinstruction =
  | PIexpr of pexpr
  | PImutExAssign of int * pexpr
  | PImutStAssign of int * ident * ((ident * pexpr) list )
  | PIexAssign of int * pexpr
  | PIstAssign of int * ident * ((ident * pexpr) list )
  | PIwhile of pexpr * pblock
  | PIreturn of pexpr
  | PIreturnNull
  | PISreturn of pcondition
  | PIcond of pcondition
and pexpr =
  | PEconst of int
  | PEbool of int
  | PEvar of int * int
  | PEbinop of binop * pexpr * pexpr
  | PEunop of unop * pexpr
  | PEstruct of pexpr * int * int * int
  | PElength of pexpr
  | PEindex of pexpr * pexpr * int
  | PEprint of string
  | PEcall of ident * pexpr list * int
  | PEvector of int * int
  | PEblock of pblock
and pblock =
  | PCFullBlock of pinstruction list * pexpr
  | PCBlock of pinstruction list
and pcondition =
  | PCif of pexpr * pblock * pblock
  | PCnestedIf of pexpr * pblock * pcondition

type decl_pfun = {
  name_pfunc : string;
  size_pfunc : int;
  return_pfunc : typ;
  body_pfunc : pblock;
}

type pdecl = PDecl_fun of decl_pfun * int
            | Decl_struct of decl_struct

type program = pdecl list
