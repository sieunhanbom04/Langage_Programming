
%{
  open Ast
%}

%token <int> CST
%token <string> IDENT
%token LEFTPAR RIGHTPAR BEGIN END COMMA SEMICOLON
%token FUN ELSE FALSE IF LET MUT RETURN STRUCT TRUE WHILE PRINT VEC
%token ASSIGN
%token OR
%token AND
%token EQUAL GQ GEQ LQ LEQ NE
%token PLUS MINUS TIMES DIV MOD
%token EXCL
%token IMPLY
%token <string> CHAIN
%token COLON
%token ARROW
%token EOF
(*
%token ! * - & &mut
%token []
%token .
*)

/* Les priorités et associativités des tokens */


/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */

%type <Ast.program> prog

%%

prog:
  decls = decl*
  EOF
  { decls }

typ:
  | t = IDENT { Tident(t) }

argument:
  MUT? name = IDENT COLON typs = typ
  { name * typs }

struct_argument:
  name = IDENT COLON typs = typ
  { name * typs }

decl:
  | STRUCT s = IDENT BEGIN t = separated_list(COMMA,struct_argument) END
  { { name = s;
     defs = t; } }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR IMPLY t = typ block
    { {
      name = f;
      defs = para;
      return = t;
      body = block
    } }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) block
    { {
      name = f;
      defs = para;
      return = TNull;
      body = block;
    } }

block:
  | BEGIN ins = instruction* expr END {CFullBlock(ins,expr)}
  | BEGIN ins = instruction* END {CBlock(ins)}

instruction:
  | e = expr SEMICOLON {IExpr(e)}
  (*à complet*)

expr:
  | PRINT EXCL LEFTPAR s = CHAIN RIGHTPAR {Eprint(s)}
