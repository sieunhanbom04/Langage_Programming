exception Error

type token = 
  | WHILE
  | VEC
  | TRUE
  | TIMES
  | STRUCT
  | SEMICOLON
  | RIGHTPAR
  | RETURN
  | PRINT
  | PLUS
  | OR
  | NE
  | MUT
  | MOD
  | MINUS
  | LQ
  | LET
  | LEQ
  | LEFTPAR
  | IMPLY
  | IF
  | IDENT of (string)
  | GQ
  | GEQ
  | FUN
  | FALSE
  | EXCL
  | EQUAL
  | EOF
  | END
  | ELSE
  | DIV
  | CST of (int)
  | COMMA
  | COLON
  | CHAIN of (string)
  | BEGIN
  | ASSIGN
  | ARROW
  | AND


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)