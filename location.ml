
(*location which is used to be outputed*)

open Lexing
open Parsing
open Printf

type location = Lct of position * position
  (*position is in-built in Lexing, including 4 tuples:
    pos_cnum: character position

    pos_bol: line position

    pos_lnum: line number

    pos_fname: name of the current file

    to get the character number, use pos_cnum - pos_bol
  *)

(*this function will take two locations and output the error we need*)

let print_location ff l =
  let l1, l2 = match l with
  | Lct (l1,l2) -> l1,l2
  in
  let cnum1 = l1.pos_cnum in
  let cnum2 = l2.pos_cnum in
  let bol1 = l1.pos_bol in
  let bol2 = l2.pos_bol in
  let fname = l1.pos_fname in
  let lnum1 = l1.pos_lnum in
  let lnum2 = l2.pos_lnum in
  let cpos1 = cnum1 - bol1 in
  let cpos2 = cnum2 - bol2 in

  if l2 > l1 then
    fprintf ff "File \"%s\", line %d, character %d- line %d, character %d\n" fname lnum1 cpos1 lnum2 cpos2
  else
    fprintf ff "File \"%s\", line %d, characters %d - %d \n" fname lnum1 cpos1 cpos2
