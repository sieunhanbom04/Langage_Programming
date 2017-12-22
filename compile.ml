open Format
open X86_64
open Ast
open Ast_code
open Tc
open Printf

exception VarUndef of string
exception FieldUndef of string

let ref frame_size = 0

type structure = { total: int;
                   fields: (string * int) list}

let ref chaine = 0
(*the total size of *)

module Vmap = Map.Make(String)

type alloc = int Vmap.t

let representation_env = (Hashtbl.create 20 : (string,structure) Hashtbl.t);;

let struct_env = (Hashtbl.create 20 : (string, (string * int) list) Hashtbl.t);;

let function_env = (Hashtbl.create 20 : (string,int) Hashtbl.t);;

let function_size_env = (Hashtbl.create 20 : (string,int) Hashtbl.t);;

let variable_env = (Hashtbl.create 20 : (string,string) Hashtbl.t);;

(*find the label for string (necessary to be distinct from each other and name of functions)*)

let rec find_next_global n i =
  let t = if i = 1 then "S" else "T" in
  if not (Hashtbl.mem function_env (t ^ string_of_int(n))) then
          (Hashtbl.add function_env (t ^ string_of_int(n)) 0; (t ^ string_of_int(n)))
  else find_next_global (n + 1) i

(*Calculate the size of each type*)

let rec calculate_size typ =
  match typ with
  | Tnull -> 0
  | Tint | Tbool -> 8
  | Tstruct id -> (try let t = Hashtbl.find representation_env id in t.total with Not_found -> raise (VarUndef(id)))
  | Tstructgeneric _ -> 16
  | Tref t -> 8
  | Tmut t -> calculate_size t

(*Convert from type struct into name of struct*)
(*TODO:: added a boolean to know dereference*)
let rec auto_dereference t =
  match t with
  | Tref (Tmut i) -> i (*true*)
  | Tstruct _ | Tstructgeneric _ -> t (*false*)
  | Tref i -> i (*true*)


let convert_into_struct_name ty =
  match ty with
  | Tstruct id -> id
  | Tstructgeneric (id, t) -> id

(* Calculate the offset from the pointer to the address of fields in each struct *)

let rec calculate_relative_position l next =
  match l with
  | [] -> []
  | (x, y) :: rest -> ( x, next) :: (calculate_relative_position l (next + y))

(*This function is used to calculate the size of complicated struct data
and update the name and value to the hash table representation_env *)

let calculate_rep p =
  let calculate_struct decl =
    match decl with
    | Decl_fun df -> Hashtbl.add function_env df.name_func (calculate_size df.return_func);
                    let total = List.fold_left (fun x y -> x + calculate_size y.type_arg) 0 df.def_func in
                    Hashtbl.add function_size_env df.name_func total
    | Decl_struct ds -> let name = ds.name_struct in
                      let field_list = ds.def_struct in
                      let total = List.fold_left (fun x y -> x + calculate_size y.type_struct_arg) 0 field_list in
                      let fields = List.fold_right (fun y x -> (y.name_struct_arg, calculate_size y.type_struct_arg)::x) field_list [] in
                      Hashtbl.add representation_env name {total = total; fields = (calculate_relative_position fields 0)};
                      let size = List.map (fun x -> x.name_struct_arg, calculate_size x.type_struct_arg) field_list in
                      Hashtbl.add struct_env name size
  in List.map calculate_struct p

(*This function is dedicated for the declaration of function*)

let rec alloc_expr env_alloc env_typ next e =
  match e with
  | Econst i -> PEconst i, next

  | Ebool i when i = true -> PEbool 1, next

  | Ebool i when i = false -> PEbool 0, next

  | Evar x ->
    begin
    try
      let ofs_x = Vmap.find x env_alloc in
      PEvar (ofs_x, calculate_size (type_check_expr env_typ (Evar x))), next
    with
      Not_found -> raise (VarUndef x)
    end

  | Ebinop (o,e1,e2) ->
    let exp1, fpmax1 = alloc_expr env_alloc env_typ next e1 in
    let exp2, fpmax2 = alloc_expr env_alloc env_typ next e2 in
    PEbinop (o,exp1,exp2), (max fpmax1 fpmax2)

  | Eunop (o,e) ->
    let exp,fpmax = alloc_expr env_alloc env_typ next e in
    PEunop (o, exp), fpmax

  | Estruct (e,id) -> let ty = auto_dereference (type_check_expr env_typ e) in
                      let loc,size2 =
                      (try
                        let t = convert_into_struct_name ty in
                        let temp = Hashtbl.find representation_env t in
                        let offset = List.assoc id temp.fields in
                        let temp2 = Hashtbl.find struct_env t in
                        let offset2 = List.assoc id temp2 in
                        offset, offset2
                      with
                        | _ -> raise (FieldUndef(id))
                      ) in
                      let exp, fpmax = alloc_expr env_alloc env_typ next e in

                      PEstruct(exp,calculate_size ty,loc,size2), fpmax

  (*| Elength of expr
  | Eindex of expr * expr*)

  | Eprint s -> let t = find_next_global 0 0 in Hashtbl.add variable_env t s; PEprint(t), next

  | Ecall (id, e) -> let exp, fpmax = List.fold_right
                      (fun t (exp, fpmax) -> let e1, fpmax1 = alloc_expr env_alloc env_typ next t in
                                              e1::exp ,max fpmax fpmax1) e ([],next) in
                                              PEcall (id, exp, Hashtbl.find function_env id), fpmax
  (*| Evector of expr list*)

  | Eblock b -> let t, fpmax = alloc_block env_alloc env_typ next b in
                PEblock(t), fpmax

and alloc_instruction env_alloc env_type next ins =
  match ins with
  | Iexpr e -> let t, fpmax = (alloc_expr env_alloc env_type next e) in PIexpr(t), fpmax, env_alloc

  | ImutExAssign (id, e) -> let t, fpmax = (alloc_expr env_alloc env_type next e) in
                            let te = type_check_expr env_type e in
                            Hashtbl.add env_type.evar id (te, true, Plein);
                            let size = calculate_size te in
                            PImutExAssign (- next - size, t), (max (next + size) fpmax), Vmap.add id (- next - size) env_alloc

  | IexAssign (id, e) -> let t, fpmax = (alloc_expr env_alloc env_type next e) in
                            let te = type_check_expr env_type e in
                            Hashtbl.add env_type.evar id (te, false, Plein);
                            let size = calculate_size te in
                            PIexAssign (- next - size, t), (max (next + size) fpmax), Vmap.add id (- next - size) env_alloc

  | ImutStAssign (id1, id2, e1) -> let st = Hashtbl.find representation_env id2 in
                                   Hashtbl.add env_type.evar id1 (Tstruct (id2), true, Plein);
                                   let l_exp, fpmax = List.fold_right (fun (x,y) (e,fp) ->
                                              let t1,fp1 = alloc_expr env_alloc env_type next y in
                                              let loc = List.assoc x st.fields in
                                              (loc,t1)::e, max fp fp1) e1 ([],next)
                                  in PImutStAssign ( - next - st.total, (max fpmax (next + st.total)),l_exp), (max fpmax (next + st.total)), Vmap.add id1 ( - next - st.total) env_alloc

  | IstAssign (id1, id2, e1) -> let st = Hashtbl.find representation_env id2 in
                                   Hashtbl.add env_type.evar id1 (Tstruct (id2), false, Plein);
                                   let l_exp, fpmax = List.fold_right (fun (x,y) (e,fp) ->
                                              let t1,fp1 = alloc_expr env_alloc env_type next y in
                                              let loc = List.assoc x st.fields in
                                              (loc,t1)::e, max fp fp1) e1 ([],next)
                                  in PImutStAssign ( - next - st.total, (max fpmax (next + st.total)),l_exp), (max fpmax (next + st.total)), Vmap.add id1 ( - next - st.total) env_alloc

  | Iwhile (e, b) -> let e1, fpmax1 = (alloc_expr env_alloc env_type next e) in
                       let b1, fpmax2 = (alloc_block env_alloc env_type next b) in
                       PIwhile (e1,b1), (max fpmax1 fpmax2), env_alloc

  | Ireturn e -> let e1, fpmax = (alloc_expr env_alloc env_type next e) in
                        PIreturn (e1), fpmax, env_alloc
  | _ -> raise (VarUndef("successful"))

  (*| IreturnNull -> PIreturnNull , next, env_alloc

  | Icond c -> let c1, fpmax1 = alloc_condition env_alloc env_type next c in
                PIcond(c1), fpmax1, env_alloc

  | ISreturn c -> let c1, fpmax1 = alloc_condition env_alloc env_type next c in
                PISreturn(c1), fpmax1, env_alloc*)

and alloc_block env_alloc env_type next e =
  match e with
  | CFullBlock (ins, e1) ->
    let new_env = {evar = Hashtbl.copy env_type.evar; level = 0} in
    let ins1, size, env = List.fold_left (fun (x,y,z) exp ->
            let x1, y1, z1 = alloc_instruction z new_env y exp in
            x1::x, max y y1, z1) ([],next,env_alloc) ins in
    let r, s = alloc_expr env new_env size e1 in
    PCFullBlock (List.rev ins1, r), max size s

  | CBlock ins ->
    let new_env = {evar = Hashtbl.copy env_type.evar; level = 0} in
    let ins1, size, env = List.fold_left (fun (x,y,z) exp ->
          let x1, y1, z1 = alloc_instruction z new_env y exp in
          x1::x, max y y1, z1) ([],next,env_alloc) ins in
          PCBlock(List.rev ins1), size

and alloc_condition env_alloc env_type next c =
  match c with
  | Cif (e, b1, b2) -> let e1, fpmax1 = alloc_expr env_alloc env_type next e in
                      let b1, fpmax2 = alloc_block env_alloc env_type next b1 in
                      let b2, fpmax3 = alloc_block env_alloc env_type next b2 in
                      PCif (e1, b1, b2), max (max fpmax1 fpmax2) fpmax3
  | CnestedIf (e ,b, i) -> let e1, fpmax1 = alloc_expr env_alloc env_type next e in
                          let b1, fpmax2 = alloc_block env_alloc env_type next b in
                          let i1, fpmax3 = alloc_condition env_alloc env_type next i in
                          PCnestedIf (e1,b1,i1), max (max fpmax1 fpmax2) fpmax3

(*This function used to compile the general declaration
let empty_env = { evar = Hashtbl.create 17, level = 1 }*)

let alloc_decl d =
  match d with
  | Decl_fun df -> let bl = df.body_func in
                  let n = df.name_func in
                  let ra = df.return_func in
                  let arg = df.def_func in
                  let env_type = {evar = Hashtbl.create 17; level = 1} in
                  List.iter (fun x -> Hashtbl.add env_type.evar x.name_arg (x.type_arg,x.mut_arg,Plein)) arg;
                  let env_alloc, next = List.fold_right (fun x (e,n) -> Vmap.add x.name_arg (n+calculate_size x.type_arg) e,
                                          (fprintf stdout "%d\n" (n + calculate_size x.type_arg);n + calculate_size x.type_arg)) arg (Vmap.empty, 8) in
                  let b, fmax = alloc_block env_alloc env_type 0 bl in
                  PDecl_fun ({ name_pfunc = n;
                    size_pfunc = next - 8;
                    return_pfunc = ra;
                    body_pfunc = b;}, fmax)
  | Decl_struct ds -> Decl_struct ds

let popn n = addq (imm n) (reg rsp)

let pushn n = subq (imm n) (reg rsp)

let rec memmove octet =
  match octet with
  | i when i = 0 -> nop
  | i -> movq (ind ~ofs:(i-8) rcx) (reg rdx) ++ movq (reg rdx) (ind ~ofs:(i-8) rbx) ++ memmove (octet-8)

let rec compile_expr e =
  match e with
  | PEconst i -> movq (imm i) (reg rax) ++ pushq (reg rax)
  | PEbool b -> pushq (imm b)
  | PEvar (v,size) -> pushn size ++ leaq (ind ~ofs:0 rsp) (rbx) ++ leaq (ind ~ofs:v rbp) (rcx) ++ memmove size
  | PEprint (s) ->  movq (ilab s) (reg rdi) ++ pushq (reg rax)
                  ++ movq (imm 0) (reg rax) ++ call "printf" ++ popq rax
  | PEbinop (o,exp1,exp2) -> compile_binop_expr o exp1 exp2
  | PEunop (o,e1) -> compile_unop_expr o e1
  | PEcall (id, exp, size) -> let fcall = List.fold_left (fun code e -> code ++ compile_expr e) nop exp in
                              pushn size ++ fcall ++ call id ++ popn (Hashtbl.find function_size_env id)
  | _ -> nop

and compile_ins ins =
  match ins with
  | PIexpr e -> compile_expr e
  | PIcond c -> compile_condition c
  | _ -> nop

and compile_condition c =
  match c with
  | PCif (e, block1, block2) -> let label1 = find_next_global 0 1 in
                              let label2 = find_next_global 0 1 in
                              let t1 = compile_expr e in
                              let b1 = compile_block block1 in
                              let b2 = compile_block block2 in
                              t1 ++ popq rax ++ cmpq (imm 0) (reg rax) ++ je label1 ++
                              b1 ++ jmp label2 ++ (label label1) ++ b2 ++ (label label2)
  (*| PCnestedIf e, block, cond ->*)

and compile_block bl =
  (match bl with
  | PCFullBlock (pins, e1) -> (List.fold_left (fun x y -> let t = compile_ins y in x ++ t) nop pins) ++ (compile_expr e1)
  | PCBlock pins -> List.fold_left (fun x y -> let t = compile_ins y in x ++ t) nop pins)

and compile_binop_expr op e1 e2 =
  let t1 = compile_expr e1 in
  let t2 = compile_expr e2 in
  match op with
  | Badd | Bsub | Bmul | Band | Bor -> compile_binop_simple_arithmetic op t1 t2
  | Bdiv | Bmod -> compile_binop_complex_arithmetic op t1 t2
  | Beq | Bneq | Blt | Ble | Bgt | Bge -> compile_binop_condition op t1 t2
	(*| Bassign -> compile_assign e1 t2*)

and compile_binop_simple_arithmetic op t1 t2 =
  let o = match op with
  | Badd -> addq
  | Bsub -> subq
  | Bmul -> imulq
  | Band -> andq
  | Bor -> orq
  in t1 ++ t2 ++ popq rcx ++ popq rax ++
  o (reg rcx) (reg rax) ++ pushq (reg rax)

and compile_binop_condition op t1 t2 =
  let o = match op with
  | Beq -> sete
  | Bneq -> setne
  | Blt -> setl
  | Ble -> setle
  | Bgt -> setg
  | Bge -> setge
  in
  t1 ++ t2 ++ popq rcx ++ popq rax ++
  cmpq (reg rcx) (reg rax) ++
  o (reg al) ++ movzbq (reg al) rax ++
  pushq (reg rax)

and compile_binop_complex_arithmetic op t1 t2 =
  let d = match op with
  | Bdiv -> reg rax
  | Bmod -> reg rdx
  in t1 ++ t2 ++ popq rcx ++ popq rax ++
  cqto ++ idivq (reg rcx) ++ pushq d

(*TODO:: Complete function compile_assign
and compile_assign e1 t2 =
  t2 ++ *)

(*TODO:: Complete unop operation*)
and compile_unop_expr o e =
  let op = match o with
  | Uneg -> negq
  | Unot -> notq
  (*| Unstar ->
  | Unp
  | Unmutp*)
  in
  compile_expr e ++ popq rax ++ op (reg rax) ++ pushq (reg rax)

let compile_decl d =
  match d with
  | PDecl_fun (func, fpmax) ->
    let code = (label func.name_pfunc) ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    pushn fpmax ++
    compile_block func.body_pfunc ++
    leave ++
    ret in
    if func.name_pfunc = "main" then code, nop
    else nop, code
  | Decl_struct s -> nop, nop

(*let int_expr exp =
  let rec comprec env next
    | Econst n -> movq (imm n) (reg rdi)
    | Ebool b -> let temp = if b then 1 else 0 in movq (imm temp) (reg rdi)
    | Ebinop*)

let compile_program p ofile =
  calculate_rep p;
  let p1 = List.map alloc_decl p in
  let codemain, codefun = List.fold_left (fun (codem, codef) f ->
                          let c1, c2 = compile_decl f in codem ++ c1, codef ++ c2) (nop,nop) p1 in
  let p = {text =
            globl "main" ++
            codemain ++
            codefun;
            data =
            Hashtbl.fold (fun x y z -> z ++ (label x ++ string y)) variable_env nop ++ (label "msg") ++ string "%d\n"
          }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  Format.fprintf fmt "@?";
  close_out f
