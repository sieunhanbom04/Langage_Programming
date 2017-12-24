
/* Analyseur syntaxique pour mini-Turtle */

%{
  open Ast
  open Printf

  let table_type = ["i32",Tint;
                    "bool",Tbool;
                    ]
  let find_type t = try List.assoc t table_type with _ -> Tstruct(t)

  exception Empty_block
  exception Empty_ins
  exception Syntax_Error

  let rec remove_null_instruction p =
    match p with
    | [] -> []
    | (Inothing) :: rest -> remove_null_instruction rest
    | (_ as t):: rest -> t :: (remove_null_instruction rest)

  let remove_option l =
    match l with
    | None -> []
    | Some t -> t

  let check_last_ins l = fprintf stdout "%d tung\n" (List.length l);
    if ((List.length l) = 1) then (match l with
    | (Iexpr e)::[] -> ()
    | Inothing::[] -> ()
    | _ -> raise Syntax_Error)
    else (match List.hd (List.rev (remove_null_instruction l)) with
    | Iexpr e -> ()
    | Icond c -> ()
    | _ -> raise Syntax_Error)

  let check_valid_block l =
   let t = List.map remove_null_instruction l in
   let check_small_block sb =
    match sb with
    | [] -> ()
    | r -> let d = List.hd (List.rev r) in
      (match d with
      | Iwhile _ | Icond _ -> raise Syntax_Error
      | _ -> ()
      )
   in
   List.iter check_small_block t

  let return_type l = let t = remove_null_instruction (List.concat l) in
                      let rev_t = List.rev t in
                      if (List.length t) = 0 then CBlock([])
                      else (check_valid_block (List.rev (List.tl (List.rev l)));
                      check_last_ins (List.hd (List.rev l));
                      let x = List.hd rev_t in
                        match x with
                        | Iexpr e ->  CFullBlock((List.rev (List.tl rev_t)),e)
                        | _ -> CBlock (t))

  let rec fix_return_block b name =
    match b with
    | CFullBlock (bl,e) -> CFullBlock(List.map (fun ins -> fix_return_ins ins name) bl, fix_return_expr e name)
    | CBlock bl -> CBlock(List.map (fun ins -> fix_return_ins ins name) bl)

  and fix_return_ins ins name =
    match ins with
    | Inothing -> Inothing
    | Iexpr e -> Iexpr (fix_return_expr e name)
    | ImutExAssign (id, e) -> ImutExAssign (id, fix_return_expr e name)
    | ImutStAssign (id1, id2, e) -> ImutStAssign (id1, id2, List.map (fun (id,e1) -> id, fix_return_expr e1 name) e)
    | IexAssign (id, e) -> IexAssign (id, fix_return_expr e name)
    | IstAssign (id1, id2, e) -> IstAssign (id1, id2, List.map (fun (id,e1) -> id, fix_return_expr e1 name) e)
    | Iwhile (e, bl) -> Iwhile(fix_return_expr e name,fix_return_block bl name)
    | Ireturn e -> ICreturn (name, fix_return_expr e name)
    | IreturnNull -> ICreturnNull name
    | Icond c -> Icond (fix_return_condition c name)

  and fix_return_expr ex name =
    match ex with
    | Ebinop (bnop, e1, e2) -> Ebinop(bnop, fix_return_expr e1 name, fix_return_expr e2 name)
    | Eunop (u,e) -> Eunop (u,fix_return_expr e name)
    | Estruct (e,id) -> Estruct (fix_return_expr e name,id)
    | Elength e -> Elength (fix_return_expr e name)
    | Eindex (e1,e2) -> Eindex (fix_return_expr e1 name, fix_return_expr e2 name)
    | Ecall (id, e) -> Ecall(id, List.map (fun e1 -> fix_return_expr e1 name) e)
    | Evector e -> Evector (List.map (fun e1 -> fix_return_expr e1 name) e)
    | Eblock bl -> Eblock (fix_return_block bl name)
    | _ -> ex

  and fix_return_condition c name =
    match c with
    | Cif (e,b1,b2) -> Cif(fix_return_expr e name, fix_return_block b1 name, fix_return_block b2 name)
    | CnestedIf (e,b,c1) -> CnestedIf (fix_return_expr e name, fix_return_block b name, fix_return_condition c1 name)

  let fix_return df = let bl = fix_return_block df.body_func df.name_func in
                      {name_func = df.name_func;
                      def_func = df.def_func;
                      return_func = df.return_func;
                      body_func = bl}
  (*let remove_option l =
    match l with
    | None -> []
    | Some t -> t
    let return_type l =
                      let t = remove_null_instruction (List.concat  l) in
                      let rev_t = List.rev t in
                      if (List.length t) = 0 then CBlock([])
                      else
                      (let x = List.hd rev_t in
                        match x with
                        | Iexpr e ->  CFullBlock((List.rev (List.tl rev_t)),e)
                        | _ -> CBlock (t))*)

%}

%token <int> CST
%token <string> IDENT
%token LEFTPAR RIGHTPAR BEGIN END COMMA SEMICOLON
%token FUN ELSE FALSE IF LET MUT RETURN STRUCT TRUE WHILE
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
  | name = IDENT LQ t = typ GQ { Tstructgeneric(name,t) }
  | POINTER t = typ {Tref(t)}
  | POINTER MUT t = typ {Tref (Tmut(t))}
;

argument:
  | MUT name = IDENT COLON typs = typ    { { name_arg = name; type_arg = typs; mut_arg = true} }
  | name = IDENT COLON typs = typ    { { name_arg = name; type_arg = typs; mut_arg = false} }
;

struct_argument:
  name = IDENT COLON typs = typ
  { { name_struct_arg = name;
      type_struct_arg = typs; } }
;

decl:
  | STRUCT s = IDENT BEGIN t = separated_list(COMMA,struct_argument) END
  { let st = { name_struct = s;
     def_struct = t; }
     in Decl_struct(st) }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR ARROW t = typ bl = block
    { let fn = {
      name_func = f;
      def_func = para;
      return_func = t;
      body_func = bl
    }
    in Decl_fun(fix_return fn) }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR bl = block
    { let fn = {
      name_func = f;
      def_func = para;
      return_func = Tnull;
      body_func = bl;
    }
    in Decl_fun(fix_return fn) }
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
  | t = IDENT EXCL LEFTSQ e = separated_list(COMMA,expr) RIGHTSQ { if t = "vec" then Evector(e) else raise Syntax_Error}
  | e1 = IDENT LEFTPAR e = separated_list(COMMA,expr) RIGHTPAR {Ecall(e1,e)}
  | LEFTPAR e = expr RIGHTPAR {e}
  | t = IDENT EXCL LEFTPAR s = CHAIN RIGHTPAR { if t = "print" then Eprint(s) else raise Syntax_Error}
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
  | EXCL {Unot}
  | TIMES {Unstar}
  | POINTER {Unp}
| POINTER MUT {Unmutp}
