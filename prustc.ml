open Lexing
open Format
open Parser
open Ast
open Printf
open Location
open Compile
open Tcstatic
open Tcresource

let length = Array.length Sys.argv
let () = (if length > 3 then (print_endline  "Prustc Error : too many arguments in call"; exit 2))


type parameter = Nothing | Parse_only | Type_only | No_asm

(*let filename = "tests/exec/vec.rs"*)

let () = if length < 2 then ((print_endline "Prustc Error : No file given to compile"); exit 2)

let filename = Sys.argv.(1)

let param = if length >= 3 then (match Sys.argv.(2) with 
	|"--parse-only" -> Parse_only

	|"--type-only" -> Type_only

	|"--no-asm" -> No_asm

	| _ -> (print_endline ("Prustc Error : unrecognized argument : "^Sys.argv.(2)); print_endline "Possible arguments are : --parse-only	--type-only	--no-asm"; exit 2))

						else Nothing

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename l (c-1) c

let localisation_modify pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename (l-1) (c-1) c



let () =
  let f = open_in filename in
  let buf = Lexing.from_channel f in
  try
    let p = Parser.prog Lexer.token buf in
    close_in f;

    if param = Parse_only then exit 0;

    let p1 = gen_type_check p in

    if param = Type_only then exit 0;

    resource_type_check p1 ;

    if param = No_asm then exit 0;

    compile_program p1 (Filename.chop_suffix filename ".rs" ^ ".s");
  with
    | Lexer.Lexing_error c -> localisation (Lexing.lexeme_start_p buf);
	                             eprintf "Erreur lexicale: %s@.\n" c;
	                             exit 1
    | Parser.Error -> localisation (Lexing.lexeme_start_p buf);
	                       eprintf "Erreur syntaxique@.\n";
	                       exit 1
		| Type_Error (s,l) -> print_endline ("Type Error : "^s); print_location stdout l;
													exit 1
		| RType_Error (s,l) -> print_endline ("Resource Type Error : "^s); print_location stdout l;
													exit 1             
	  | _ -> print_endline "Unknown error";
	         exit 2
	         
