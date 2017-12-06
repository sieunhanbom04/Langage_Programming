
(*open Format
open X86_64
open Ast*)

exception VarUndef of string

let ref frame_size = 0

let ref chaine = 0
(*the total size of *)

let offset = (Hashtbl.create 100 : (string * string, int) Hashtbl.t);;

let int_expr exp =
  let rec comprec env next
    | Econst n -> movq (imm n) (reg rdi)
    | Ebool b -> let temp = if b then 1 else 0 in movq (imm temp) (reg rdi)
    | Ebinop

let int_func f env =
  |

let compile_program p ofile =
