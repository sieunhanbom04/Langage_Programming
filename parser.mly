
%{
  open Ast
  let table_type = ["i32",Tint;
                    "bool",Tbool;
                    ]
  let find_type t = try List.assoc t table_type with _ -> Tstruct(t)

  exception Empty_block
  exception Empty_ins
  exception Syntax_Error

  let return_type l = let t = List.concat l in
                      let rev_t = List.rev t in
                      if (List.length t) = 0 then CBlock([])
                      else
                      (let x = List.hd rev_t in
                        match x with
                        | Inothing -> CBlock (t)
                        | Iexpr e ->  CFullBlock(List.rev (List.tl rev_t),e)
                        | _ -> raise Syntax_Error)
                        (*| Iexpr e ->  CFullBlock(List.rev (List.tl rev_t),e)*)
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
%token ARROW
%token <string> CHAIN
%token COLON
%token EOF
%token POINTER
%token LEFTSQ RIGHTSQ
%token LEN
%token POINT


/* Les priorités et associativités des tokens */
%right ASSIGN
%left OR
%left AND
%nonassoc EQUAL GQ GEQ LQ LEQ NE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary
%nonassoc length
%nonassoc st

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */

%type <Ast.program> prog

%%

prog:
  decls = decl* EOF
  { decls }
;

typ:
  | t = IDENT { find_type t }
  | POINTER t = typ {Tref(t)}
  | POINTER MUT t = typ {Tmut(t)}
;

argument:
  | MUT name = IDENT COLON typs = typ    { { name = name; types = typs; mut = true} }
  | name = IDENT COLON typs = typ    { { name = name; types = typs; mut = false} }
;

struct_argument:
  name = IDENT COLON typs = typ
  { { name = name;
      types = typs; } }
;

decl:
  | STRUCT s = IDENT BEGIN t = separated_list(COMMA,struct_argument) END
  { let st = { name = s;
     defs = t; }
     in Decl_struct(st) }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR ARROW t = typ bl = block
    { let fn = {
      name = f;
      defs = para;
      return = t;
      body = bl
    }
    in Decl_fun(fn) }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR bl = block
    { let fn = {
      name = f;
      defs = para;
      return = Tnull;
      body = bl;
    }
    in Decl_fun(fn) }
;

block:
  | BEGIN ins = separated_list(SEMICOLON,instruction_in_general) END { return_type ins }
;

assign:
  name = IDENT COLON e = expr {(name , e)}
;
instruction_without_block:
  | {Inothing}
  | e = expr {Iexpr(e)}
  | LET MUT id = IDENT ASSIGN e = expr {ImutExAssign(id , e)}
  | LET id = IDENT ASSIGN e = expr {IexAssign(id,e)}
  | LET MUT id = IDENT ASSIGN idstruct = IDENT BEGIN var = separated_list(COMMA, assign) END
      {ImutStAssign(id, idstruct, var)}
  | LET id = IDENT ASSIGN idstruct = IDENT BEGIN var = separated_list(COMMA, assign) END
      {IstAssign(id, idstruct, var)}
  | RETURN {IreturnNull}
  | RETURN e = expr {Ireturn(e)}
;
instruction_with_block:
  | WHILE e = expr bl = block {Iwhile(e,bl)}
  | i = condition {Icond(i)}
;
instruction_in_general:
  bad_instruction = instruction_with_block* good_instruction = instruction_without_block {bad_instruction @ (good_instruction::[])}
;
(*instruction:
  | SEMICOLON {Inothing}
  | e = expr SEMICOLON {Iexpr(e)}
  | LET MUT id = IDENT ASSIGN e = expr SEMICOLON {ImutExAssign(id , e)}
  | LET id = IDENT ASSIGN e = expr SEMICOLON {IexAssign(id,e)}
  | LET MUT id = IDENT ASSIGN idstruct = IDENT BEGIN var = separated_list(COMMA, assign) END SEMICOLON
      {ImutStAssign(id, idstruct, var)}
  | LET id = IDENT ASSIGN idstruct = IDENT BEGIN var = separated_list(COMMA, assign) END SEMICOLON
      {IstAssign(id, idstruct, var)}
  | WHILE e = expr bl = block {Iwhile(e,bl)}
  | RETURN SEMICOLON {IreturnNull}
  | RETURN e = expr SEMICOLON {Ireturn(e)}
  | i = condition {Icond(i)}
;*)

condition:
  | IF e = expr bl = block {Cif(e, bl, CBlock([]))}
  | IF e = expr bl1 = block ELSE bl2 = block { Cif(e, bl1, bl2) }
  | IF e = expr bl = block ELSE i = condition { CnestedIf(e, bl, i) }
;

expr:
  | i = CST { Econst(i) }
  | TRUE { Ebool(true) }
  | FALSE { Ebool(false) }
  | id = IDENT { Evar(id) }
  | bl = block {Eblock(bl)}
  | e1 = expr op = bop e2 = expr { Ebinop(op,e1,e2) }
  | op = unop e = expr %prec unary { Eunop(op,e) }
  | e1 = expr POINT id = IDENT %prec st { Estruct(e1,id) }
  | e = expr POINT LEN LEFTPAR RIGHTPAR %prec length { Elength(e) }
  | e1 = expr LEFTSQ e2 = expr RIGHTSQ { Eindex(e1,e2) }
  | VEC EXCL LEFTSQ e = separated_list(COMMA,expr) RIGHTSQ {Evector(e)}
  | e1 = IDENT LEFTPAR e = separated_list(COMMA,expr) RIGHTPAR {Ecall(e1,e)}
  | LEFTPAR e = expr RIGHTPAR {e}
  | PRINT EXCL LEFTPAR s = CHAIN RIGHTPAR {Eprint(s)}
  (*à complet*)
;

%inline bop:
  | PLUS {Badd}
  | MINUS {Bsub}
  | TIMES {Bmul}
  | DIV {Bdiv}
  | MOD {Bmod}
  | EQUAL {Beq}
  | NE {Bneq}
  | LQ {Blt}
  | LEQ {Ble}
  | GQ {Bgt}
  | GEQ {Bge}
  | AND {Band}
  | OR {Bor}
  | ASSIGN {Bassign}
;

%inline unop:
  | MINUS {Uneg}
  | TIMES {Unstar}
  | POINTER {Unp}
  | POINTER MUT {Unmutp}
