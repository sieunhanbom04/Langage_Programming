open Lexing
open Format
open Parser

let () =
  let f = open_in "tung.txt" in
  let buf = Lexing.from_channel f in
  let p = Parser.prog Lexer.token buf in
  close_in f;
