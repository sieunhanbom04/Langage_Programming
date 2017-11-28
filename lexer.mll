{
  open Lexing
  (*open Parser*)

  exception Lexing_error of string

  type token = CST of int
              | IDENT of string
              | PLUS | MINUS | TIMES | DIV | MOD
              | LEFTPAR | RIGHTPAR | BEGIN | END | COMMA | SEMICOLON
              | FUN | ELSE | FALSE | IF | LET | MUT | RETURN | STRUCT | TRUE | WHILE
              | ASSIGN | OR | AND
              | EQUAL | GQ | GEQ | LQ | LEQ | NE | EOF
              | EXCL | QUO
  let table_kw =
  ["fn",FUN;
   "else",ELSE;
   "false",FALSE;
   "if",IF;
   "let",LET;
   "mut",MUT;
   "return",RETURN;
   "struct",STRUCT;
   "true",TRUE;
   "while",WHILE;
   "print",PRINT;
   "vec",VEC]

   let assoc_variable s = try List.assoc s table_kw with Not_found -> IDENT (s)
}

let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | chiffre | '_')*
let entier = chiffre+
let caractere = [^ '\\' '"'] | "\\" | "\\\"" | "\\n"
let chain = '"' caractere* '"'
let space = [' ' '\t']

rule token = parse
  | "//" [^ '\n']* '\n'
  | '\n'    { new_line lexbuf; token lexbuf }
  | ident as t { assoc_variable t }
  | space+ {token lexbuf}
  | entier as t {CST (int_of_string t)}
  | "==" {EQUAL}
  | "!=" {NE}
  | ">=" {GEQ}
  | ">" {GQ}
  | "<=" {LEQ}
  | "<" {LQ}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIV}
  | "%" {MOD}
  | "(" {LEFTPAR}
  | ")" {RIGHTPAR}
  | "{" {BEGIN}
  | "}" {END}
  | "," {COMMA}
  | ";" {SEMICOLON}
  | "=" {ASSIGN}
  | "||" {OR}
  | "&&" {AND}
  | ":" {COLON}
  | '"' (caractere* as t) '"' {CHAIN(t)}
  | "!" {EXCL}
  | "->" {IMPLY}
  | eof {EOF}
  | _ as t {raise (Lexing_error (String.make 1 t))}

(*
rule token = parse
  | "//" [^ '\n']* '\n'
  | '\n'    { new_line lexbuf; token lexbuf }
  | ident as t { print_endline t; token lexbuf }
  | space+ {token lexbuf}
  | entier as t {print_int (int_of_string t); print_endline ""; token lexbuf}
  | "==" {print_endline "=="; token lexbuf}
  | "!=" {print_endline "!="; token lexbuf}
  | ">=" {print_endline ">="; token lexbuf}
  | ">" {print_endline ">"; token lexbuf}
  | "<=" {print_endline "<="; token lexbuf}
  | "<" {print_endline "<";token lexbuf}
  | '+' {print_endline "+"; token lexbuf}
  | '-' {print_endline "-"; token lexbuf}
  | '*' {print_endline "*"; token lexbuf}
  | '/' {print_endline "/"; token lexbuf}
  | '%' {print_endline "%"; token lexbuf}
  | '(' {print_endline "("; token lexbuf}
  | ')' {print_endline ")"; token lexbuf}
  | '{' {print_endline "{"; token lexbuf}
  | '}' {print_endline "}"; token lexbuf}
  | ',' {print_endline ","; token lexbuf}
  | ';' {print_endline ";"; token lexbuf}
  | "=" {print_endline "="; token lexbuf}
  | "||" {print_endline "||"; token lexbuf}
  | "&&" {print_endline "&&"; token lexbuf}
  | "!" {print_endline "ONLY"; token lexbuf}
  | chain {print_endline "\""; token lexbuf}
  | eof {()}
  | _ as t {raise (Lexing_error (String.make 1 t))}

{let () = token (Lexing.from_channel stdin)}*)
