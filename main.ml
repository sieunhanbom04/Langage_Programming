open Lexing
open Format
open Parser
open Ast

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" "tung.txt" l (c-1) c

let print_expr e =
  match e with
  | Eprint s -> print_endline s
  | _ -> ()

let rec print_ins ins =
  match ins with
  | (IExpr e)::[] -> print_expr e
  | _ -> ()

let print_block b =
  match b with
  | CBlock bgod -> print_ins bgod
  | _ -> ()

let rec print_ast p =
  match p with
  | [] -> ()
  | (Decl_fun f)::[] -> let t = f.body in print_block t
  | (Decl_struct s)::[] -> ()

let () =
  let f = open_in "tung.txt" in
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
