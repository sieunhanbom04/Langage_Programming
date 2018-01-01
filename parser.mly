
/* Analyseur syntaxique pour mini-Turtle */

%{
  open Ast
  open Printf
  open Parsing
  open Location

  let table_type = ["i32",Tint;
                    "bool",Tbool;
                    ]
  let find_type t = try List.assoc t table_type with _ -> Tstruct(t)

  exception Syntax_Error of location * string

  let rec remove_null_instruction p =
    match p with
    | [] -> []
    | (Inothing _) :: rest -> remove_null_instruction rest
    | (_ as t):: rest -> t :: (remove_null_instruction rest)

  let check_last_ins l =
    if ((List.length l) = 1) then (match l with
    | (Iexpr (e,l))::[] -> ()
    | (Inothing l)::[] -> ()
    | _ -> raise Parsing.Parse_error)
    else (match List.hd (List.rev (remove_null_instruction l)) with
    | Iexpr (e,l) -> ()
    | Icond (c,l) -> ()
    | Iwhile (e,b,l) -> ()
    | _ -> raise Parsing.Parse_error)

  (*let check_valid_block l =
   let t = List.map remove_null_instruction l in
   let check_small_block sb =
    match sb with
    | [] -> ()
    | r -> let d = List.hd (List.rev r) in
      (match d with
      | Iwhile _ | Icond _ -> raise (Syntax_Error 3)
      | _ -> ()
      )
   in
   List.iter check_small_block t*)

  let return_type l loc = let t = remove_null_instruction (List.concat l) in
                      let rev_t = List.rev t in
                      if (List.length t) = 0 then CBlock([],loc)
                      else (*check_valid_block (List.rev (List.tl (List.rev l)));*)
                      (try (check_last_ins (List.hd (List.rev l)));
                      (let x = List.hd (List.rev (List.concat l)) in
                        match x with
                        | Iexpr (e,l) ->  CFullBlock(remove_null_instruction (List.rev (List.tl rev_t)),e,loc)
                        | _ -> CBlock (t,loc))
                      with _ -> raise (Syntax_Error (loc, "Syntax Error With Block")))

  let rec fix_return_block b name =
    match b with
    | CFullBlock (bl,e,l) -> CFullBlock(List.map (fun ins -> fix_return_ins ins name) bl, fix_return_expr e name,l)
    | CBlock (bl,l) -> CBlock(List.map (fun ins -> fix_return_ins ins name) bl,l)

  and fix_return_ins ins name =
    match ins with
    | Inothing l -> Inothing l
    | Iexpr (e,l) -> Iexpr (fix_return_expr e name,l)
    | IexAssign (id, e, bo, l) -> IexAssign (id, fix_return_expr e name, bo, l)
    | IstAssign (id1, id2, e, bo, l) -> IstAssign (id1, id2, List.map (fun (id,e1) -> id, fix_return_expr e1 name) e, bo, l)
    | Iwhile (e, bl, l) -> Iwhile(fix_return_expr e name,fix_return_block bl name, l)
    | Ireturn (e, l) -> ICreturn (name, fix_return_expr e name, l)
    | IreturnNull l -> ICreturnNull (name, l)
    | Icond (c, l) -> Icond (fix_return_condition c name, l)

  and fix_return_expr ex name =
    match ex with
    | Ebinop (bnop, e1, e2, l) -> Ebinop(bnop, fix_return_expr e1 name, fix_return_expr e2 name, l)
    | Eunop (u,e, l) -> Eunop (u,fix_return_expr e name, l)
    | Estruct (e, id, l) -> Estruct (fix_return_expr e name,id, l)
    | Elength (e,l) -> Elength (fix_return_expr e name, l)
    | Eindex (e1, e2, l) -> Eindex (fix_return_expr e1 name, fix_return_expr e2 name, l)
    | Ecall (id, e, l) -> Ecall(id, List.map (fun e1 -> fix_return_expr e1 name) e, l)
    | Evector (e, l) -> Evector (List.map (fun e1 -> fix_return_expr e1 name) e, l)
    | Eblock (bl, l) -> Eblock (fix_return_block bl name, l)
    | _ -> ex

  and fix_return_condition c name =
    match c with
    | Cif (e,b1,b2, l) -> Cif(fix_return_expr e name, fix_return_block b1 name, remove_option b2 name, l)
    | CnestedIf (e,b,c1,l) -> CnestedIf (fix_return_expr e name, fix_return_block b name, fix_return_condition c1 name,l)

  and remove_option bl name =
    match bl with
    | None -> bl
    | Some s -> Some (fix_return_block s name)

  let fix_return df = let bl = fix_return_block df.body_func df.name_func in
                      {name_func = df.name_func;
                      def_func = df.def_func;
                      return_func = df.return_func;
                      body_func = bl;
                      loc_func = df.loc_func}

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
  | MUT name = IDENT COLON typs = typ    { { name_arg = name;
                                             type_arg = typs;
                                             mut_arg = true;
                                             loc_arg = Lct($startpos,$endpos)} }
  | name = IDENT COLON typs = typ    { { name_arg = name;
                                         type_arg = typs;
                                         mut_arg = false;
                                         loc_arg = Lct($startpos,$endpos)} }
;

struct_argument:
  name = IDENT COLON typs = typ
  { { name_struct_arg = name;
      type_struct_arg = typs;
      loc_struct_arg = Lct($startpos,$endpos) } }
;

decl:
  | STRUCT s = IDENT BEGIN t = separated_list(COMMA,struct_argument) END
  { let st = { name_struct = s;
     def_struct = t;
     loc_struct = Lct($startpos,$endpos); }
     in Decl_struct(st) }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR ARROW t = typ bl = block
    { let fn = {
      name_func = f;
      def_func = para;
      return_func = t;
      body_func = bl;
      loc_func = Lct($startpos,$endpos);
    }
    in Decl_fun(fix_return fn) }
  | FUN f = IDENT LEFTPAR  para = separated_list(COMMA, argument) RIGHTPAR bl = block
    { let fn = {
      name_func = f;
      def_func = para;
      return_func = Tnull;
      body_func = bl;
      loc_func = Lct($startpos,$endpos);
    }
    in Decl_fun(fix_return fn) }
;

block:
  | BEGIN ins = separated_list(SEMICOLON,instruction_in_general) END { return_type ins (Lct($startpos,$endpos))} (*TODO*)
;

assign:
  name = IDENT COLON e = expr {(name , e)}
;
instruction_without_block:
  | {Inothing (Lct($startpos,$endpos))}

  | e = expr {Iexpr(e, Lct($startpos,$endpos))}

  | LET MUT id = IDENT ASSIGN e = expr {IexAssign(id , e, true, Lct($startpos,$endpos))}

  | LET id = IDENT ASSIGN e = expr {IexAssign(id, e, false, Lct($startpos,$endpos))}

  | LET MUT id = IDENT ASSIGN idstruct = IDENT BEGIN var = separated_list(COMMA, assign) END
      {IstAssign(id, idstruct, var, true, Lct($startpos,$endpos))}

  | LET id = IDENT ASSIGN idstruct = IDENT BEGIN var = separated_list(COMMA, assign) END
      {IstAssign(id, idstruct, var, false, Lct($startpos,$endpos))}

  | RETURN { IreturnNull (Lct ($startpos,$endpos)) }

  | RETURN e = expr {Ireturn(e, Lct($startpos,$endpos))}
;
instruction_with_block:
  | WHILE e = expr bl = block {Iwhile(e,bl, Lct($startpos,$endpos))}
  | i = condition {Icond(i, Lct($startpos,$endpos))}
;
instruction_in_general:
  bad_instruction = instruction_with_block* good_instruction = instruction_without_block {bad_instruction @ (good_instruction::[])}
;

condition:
  | IF e = expr bl = block {Cif(e, bl, None, Lct($startpos,$endpos))}

  | IF e = expr bl1 = block ELSE bl2 = block { Cif(e, bl1, Some (bl2), Lct($startpos,$endpos)) }

  | IF e = expr bl = block ELSE i = condition { CnestedIf(e, bl, i, Lct($startpos,$endpos)) }
;

expr:
  | i = CST { Econst(i, Lct($startpos,$endpos)) }

  | TRUE { Ebool(true, Lct($startpos,$endpos)) }

  | FALSE { Ebool(false, Lct($startpos,$endpos)) }

  | id = IDENT { Evar(id, Lct($startpos,$endpos)) }

  | bl = block {Eblock(bl, Lct($startpos,$endpos))}

  | e1 = expr op = bop e2 = expr { Ebinop(op,e1,e2, Lct($startpos,$endpos)) }

  | op = unop e = expr %prec unary { Eunop(op,e, Lct($startpos,$endpos)) }

  | e1 = expr POINT id = IDENT %prec st { Estruct(e1,id, Lct($startpos,$endpos)) }

  | e = expr POINT id = IDENT LEFTPAR RIGHTPAR %prec length { if id = "len" then Elength(e, Lct($startpos,$endpos))
                                                              else raise (Syntax_Error(Lct($startpos,$endpos), "Expected keywork len"))}

  | e1 = expr LEFTSQ e2 = expr RIGHTSQ { Eindex(e1, e2, Lct($startpos,$endpos)) }

  | t = IDENT EXCL LEFTSQ e = separated_list(COMMA,expr) RIGHTSQ { if t = "vec" then Evector(e, Lct($startpos,$endpos))
                                                                    else raise (Syntax_Error(Lct($startpos,$endpos), "Expected keywork vec"))}

  | e1 = IDENT LEFTPAR e = separated_list(COMMA,expr) RIGHTPAR {Ecall(e1, e, Lct($startpos,$endpos))}

  | LEFTPAR e = expr RIGHTPAR {e}

  | t = IDENT EXCL LEFTPAR s = CHAIN RIGHTPAR { if t = "print" then Eprint(s, Lct($startpos,$endpos))
                                                else raise (Syntax_Error(Lct($startpos,$endpos), "Expected keywork print"))}
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
