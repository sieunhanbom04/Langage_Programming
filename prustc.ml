open Lexing
open Format
open Parser
open Ast
open Printf
open Location
open Compile
open Tcstatic
open Tcresource

let filename = "tung.rs"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename l (c-1) c

let localisation_modify pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename (l-1) (c-1) c

(*let rec print_expr e =
  match e with
  | Eprint s -> print_endline s
  | Eunop (e1, e2) -> if e1 = Uneg then (fprintf stdout "-"; print_expr e2)
  | Econst e1 -> (fprintf stdout "%d\n" e1)
  | Evar id -> (fprintf stdout "%s\n" id)
  | Estruct (e1,id) -> print_expr e1; (fprintf stdout "%s\n" id)
  | Ebool b -> (fprintf stdout "%s\n" (string_of_bool b))
  | Ebinop (b,e1,e2) -> print_expr e1; print_expr e2
  | Ecall(id,e1) -> print_endline id; List.iter print_expr e1
  | _ -> ()

let print_ins ins =
  let print_output inst =
    match inst with
    | Inothing -> fprintf stdout "nothing"
    | Iexpr e -> print_expr e
    | IexAssign (id,e) -> print_expr e
    | ICreturn (f,e1) -> fprintf stdout "%s\n" f; print_expr e1
    | ICreturnNull f -> fprintf stdout "%s\n" f
    | IstAssign (id1, id2, t) -> (fprintf stdout "%s\n" id1); (fprintf stdout "%s\n" id2);
    | _ -> ()
  in
  List.iter print_output ins

let print_block b =
  match b with
  | CBlock bgod -> print_ins (bgod); print_endline "check_block"
  | CFullBlock (bgod,exp) -> print_ins bgod; print_expr exp; print_endline "check_fullblock"

let rec print_ast p =
  let print_decl t =
    match t with
    | Decl_fun f -> let t = f.body_func in print_block t; (*print_endline "check_ast"*)
    | Decl_struct s -> ()
    in List.iter print_decl p*)


let () =
  let f = open_in filename in
  let buf = Lexing.from_channel f in
  try
    let p = Parser.prog Lexer.token buf in
    close_in f;
    (*print_ast p;*)
    let p1 = gen_type_check p in
    resource_type_check p1 ;
    compile_program p1 (Filename.chop_suffix filename ".rs" ^ ".s");
  with
    | Lexer.Lexing_error c -> localisation (Lexing.lexeme_start_p buf);
	                             eprintf "Erreur lexicale: %s@.\n" c;
	                             exit 1
    | Parser.Error -> localisation (Lexing.lexeme_start_p buf);
	                       eprintf "Erreur syntaxique@.\n";
	                       exit 1
    (*| _ -> localisation_modify (Lexing.lexeme_start_p buf);
	                       eprintf "Erreur syntaxique 1@.\n";
	                       exit 1*)
