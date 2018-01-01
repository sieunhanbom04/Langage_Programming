open Ast

type pinstruction =
  | PIexpr of pexpr
  | PImutExAssign of int * int * pexpr
              (*the relative position with rbp, size of the expression, true pexpr*)
  | PImutStAssign of int * int * ((int * int * pexpr) list )
              (*the relative position with rbp of the assigned variable, size,list expr and offset*)
  | PIexAssign of int * int * pexpr
              (*the relative position with rbp, size of the expression, true pexpr*)
  | PIstAssign of int * int * ((int * int * pexpr) list )
              (*the relative position with rbp of the assigned variable, size,list expr and offset*)
  | PIwhile of pexpr * pblock
  | PIreturn of string * pexpr
  | PIreturnNull of string
  | PIcond of pcondition
and pexpr =
  | PEconst of int
  | PEbool of int
  | PEvar of int * int
  | PEbinop of binop * pexpr * pexpr
  | PEassign of pexpr * pexpr * int
              (*right and left expression with the size of expression*)
  | PEunop of unop * pexpr
  | PEdereference of pexpr * int
              (*expression, the true size of the variable*)
  | PEreference of int * pexpr
              (*size of expression and expression*)
  | PEstruct of pexpr * int * int * int * bool
              (*expression, size of struct, location of field, size of field, auto_dereference or not*)
  | PElength of pexpr * bool
              (*vector, auto_dereference or not*)
  | PEindex of pexpr * pexpr * int * bool
              (*vector, index, auto_dereference or not*)
  | PEprint of string
  | PEcall of ident * pexpr list * int
  | PEvector of int * int * pexpr list
              (*number of elements, size of index, list of expression*)
  | PEblock of pblock
and pblock =
  | PCFullBlock of pinstruction list * pexpr
  | PCBlock of pinstruction list
and pcondition =
  | PCif of pexpr * pblock * (pblock option)
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
