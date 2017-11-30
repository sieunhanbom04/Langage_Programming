open Lexing
open Format
open Parser
open Ast
open Printf

let filename = "tung.txt"
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename l (c-1) c

let rec print_expr e =
  match e with
  | Eprint s -> print_endline s
  | Eunop (e1, e2) -> if e1 = Uneg then (fprintf stdout "-"; print_expr e2)
  | Econst e1 -> (fprintf stdout "%d\n" e1)
  | Evar id -> (fprintf stdout "%s\n" id)
  | _ -> ()

let print_ins ins =
  let print_output inst =
    match inst with
    | IexAssign (id,e) -> print_expr e
    | Ireturn e1 -> print_expr e1
    | _ -> ()
  in
  List.iter print_output ins

let print_block b =
  match b with
  | CBlock bgod -> print_ins (bgod); (*print_endline ("check_block " ^ (string_of_int (List.length bgod)));*)
  | _ -> ()

let rec print_ast p =
  match p with
  | [] -> ()
  | (Decl_fun f)::[] -> let t = f.body in print_block t; (*print_endline "check_ast"*)
  | (Decl_struct s)::[] -> ()

let () =
  let f = open_in filename in
  let buf = Lexing.from_channel f in
  try
    let p = Parser.prog Lexer.token buf in
    close_in f;
    print_ast p
  with
    | Lexer.Lexing_error c -> localisation (Lexing.lexeme_start_p buf);
	                             eprintf "Erreur lexicale: %s@." c;
	                             exit 1
    | Parser.Error -> localisation (Lexing.lexeme_start_p buf);
	                       eprintf "Erreur syntaxique@.";
	                       exit 1
