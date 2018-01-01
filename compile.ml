open Format
open X86_64
open Ast
open Ast_code
open Tcstatic
open Tcresource
open Location
open Printf

exception ShouldBeDealtByTC of string
exception ErrorCompiler of location * string

let ref frame_size = 0

type structure = { total: int;
                   fields: (string * int) list}

let ref chaine = 0
(*the total size of *)

module Vmap = Map.Make(String)

type alloc = int Vmap.t

(*hash table contains the offset of fields in struct*)

let representation_env = (Hashtbl.create 20 : (string,structure) Hashtbl.t);;


(*hash table contains the size of each field in struct*)

let struct_env = (Hashtbl.create 20 : (string, (string * int) list) Hashtbl.t);;


(*hash table contains the size of return value of a function*)

let function_env = (Hashtbl.create 20 : (string,int) Hashtbl.t);;


(*hash table contains the total size of parameter of a function

also contains some label used for while and if*)

let function_size_env = (Hashtbl.create 20 : (string,int) Hashtbl.t);;


(*hash table contains the string which could be printed in the assembly*)

let variable_env = (Hashtbl.create 20 : (string,string) Hashtbl.t);;


(*hash table find the return label of each function*)

let return_label_env = (Hashtbl.create 20 : (string,string) Hashtbl.t);;

(*find the label for string (necessary to be distinct from each other and name of functions)*)

let rec find_next_global n i =
  let t = if i = 1 then "S" else if i = 2 then "L" else if i = 3 then "B" else "T" in
  if not (Hashtbl.mem function_env (t ^ string_of_int(n))) then
          (Hashtbl.add function_env (t ^ string_of_int(n)) 0; (t ^ string_of_int(n)))
  else find_next_global (n + 1) i

let rec find_return_label n name =
  if not (Hashtbl.mem function_env (name ^ string_of_int(n))) then
          (Hashtbl.add function_env (name ^ string_of_int(n)) 0;
          Hashtbl.add return_label_env name (name ^ string_of_int(n)))
  else find_return_label (n + 1) name
(*Calculate the size of each type*)

let rec calculate_size typ =
  match typ with
  | Tnull -> 8
  | Tint | Tbool -> 8
  | Tstruct id -> (try let t = Hashtbl.find representation_env id in t.total with Not_found -> raise (ShouldBeDealtByTC("undefined field" ^ id)))
  | Tstructgeneric _ -> 16
  | Tref t -> 8
  | Tmut t -> calculate_size t

(*check the auto dereference in program*)
(*reminder:: this function is modified before test vector*)
let rec auto_dereference t =
  match t with
  | Tref i -> let t1, dr = auto_dereference i in t1, true
  | Tmut x -> auto_dereference x
  | Tstruct _
  | Tstructgeneric _ -> t, false
  | _ as ty -> ty, false

(*Convert from type struct into name of struct*)

let convert_into_struct_name ty =
  match ty with
  | Tstruct id -> id
  | Tstructgeneric (id, t) -> id

(*check the type of a vector*)

let rec return_type_vector ty =
  match ty with
  | Tmut x -> return_type_vector x
  | Tref x -> return_type_vector x
  | Tstructgeneric (id,t) -> t
  | _ -> raise (ShouldBeDealtByTC("It is not a vector"))

(* Calculate the offset from the pointer to the address of fields in each struct *)

let rec calculate_relative_position l next =
  match l with
  | [] -> []
  | (x, y) :: rest -> ( x, next) :: (calculate_relative_position rest (next + y))

let print_struct l =
  let print_field t =
    fprintf stdout "%s: %d\n" (fst t) (snd t)
  in
  List.iter print_field l

(*This function is used to calculate the size of complicated struct data
and update the name and value to the hash table representation_env *)

let calculate_rep p =
  let calculate_struct decl =
    match decl with
    | TDecl_fun df -> Hashtbl.add function_env df.name_tfunc (calculate_size df.return_tfunc);
                    let total = List.fold_left (fun x y -> x + calculate_size y.type_arg) 0 df.def_tfunc in
                    Hashtbl.add function_size_env df.name_tfunc total;
                    find_return_label 0 df.name_tfunc;

    | TDecl_struct ds -> let name = ds.name_struct in
                      let field_list = ds.def_struct in
                      let start_index = if (List.length field_list) = 0 then 0
                      else -(calculate_size (List.hd field_list).type_struct_arg) in
                      let total = List.fold_left (fun x y -> x + calculate_size y.type_struct_arg)
                                          0 field_list in
                      let fields = List.fold_left
                                  (fun x y -> (y.name_struct_arg, calculate_size y.type_struct_arg)::x) [] field_list in
                      Hashtbl.add representation_env name
                      {total = total; fields = calculate_relative_position (List.rev fields) 0};
                      (*print_struct (calculate_relative_position (List.rev fields) 0);*)
                      let size = List.map (fun x -> x.name_struct_arg, calculate_size x.type_struct_arg) field_list in
                      Hashtbl.add struct_env name size

  in List.iter calculate_struct p

(*This function is dedicated for the declaration of function*)
(*let compute_address env_alloc e =
  match e with
  | Evar (x,_) ->
  begin
  try
    Vmap.find x env_alloc
  with
    Not_found -> raise (VarUndef x)
  end
  | _ -> raise GetReference*)

(*This function allocates the memory for the whole program*)

let rec alloc_expr env_alloc next e =
  match e with
  | TEconst (i,_,_) ->  PEconst i, next

  | TEbool (i,_,_) when i = true -> PEbool 1, next

  | TEbool (i,_,_) when i = false -> PEbool 0, next

  | TEvar (x,l,t) ->
    begin
    try
      let ofs_x = Vmap.find x env_alloc in
      PEvar (ofs_x, calculate_size t), next
    with
      Not_found -> raise (ErrorCompiler(l, "undefined variable" ^ x))
    end

  | TEbinop (o,e1,e2,_,_) ->
    let exp1, fpmax1 = alloc_expr env_alloc next e1 in
    let exp2, fpmax2 = alloc_expr env_alloc next e2 in
    (match o with
    | Bassign -> PEassign (exp1,exp2,calculate_size (extract_type_expr e1)), (max fpmax1 fpmax2)
    | _ -> PEbinop (o,exp1,exp2), (max fpmax1 fpmax2))

  | TEunop (o,e,_,_) ->
    let exp,fpmax = alloc_expr env_alloc next e in
    (match o with
    | Unstar -> let true_type, _ = (auto_dereference (extract_type_expr e)) in
                let size = (calculate_size true_type) in
                PEdereference (exp, size), fpmax

    | Unp | Unmutp -> let t = calculate_size (extract_type_expr e) in
                PEreference(t,exp), fpmax

    | _ -> PEunop (o, exp), fpmax)

  | TEstruct (e,id,l,_) -> let ty,de_re = auto_dereference (extract_type_expr e) in
                      let loc,size2 =
                      (try
                        let t = convert_into_struct_name ty in
                        let temp = Hashtbl.find representation_env t in
                        let offset = List.assoc id temp.fields in
                        let temp2 = Hashtbl.find struct_env t in
                        let offset2 = List.assoc id temp2 in
                        offset, offset2
                      with
                        | _ -> raise (ErrorCompiler(l,"undefined field" ^ id))
                      ) in
                      let exp, fpmax = alloc_expr env_alloc next e in

                      PEstruct(exp, calculate_size ty, loc, size2, de_re), fpmax

  | TElength (ex,_,_) -> let ty,de_re = auto_dereference (extract_type_expr ex) in
                  let exp1, fpmax = alloc_expr env_alloc next ex in
                  PElength (exp1,de_re), fpmax

  | TEvector (ev,_,_) -> let size_index = if ((List.length ev) = 0) then 0 else (calculate_size (extract_type_expr (List.hd ev))) in
                  let exp, fpmax = List.fold_right
                      (fun t (exp, fpmax) -> let e1, fpmax1 = alloc_expr env_alloc next t in
                                              e1::exp ,max fpmax fpmax1) ev ([],next) in
                                              PEvector(List.length ev, size_index, exp), fpmax

  | TEindex (exp1, exp2,_,_) -> let ty,de_re = auto_dereference (extract_type_expr exp1) in
                          let size_index = calculate_size (return_type_vector (extract_type_expr exp1)) in
                          let e1, fpmax1 = alloc_expr env_alloc next exp1 in
                         let e2, fpmax2 = alloc_expr env_alloc next exp2 in
                         PEindex(e1, e2, size_index, de_re), max fpmax1 fpmax2

  | TEprint (s,_,_) -> let t = find_next_global 0 0 in Hashtbl.add variable_env t s; PEprint(t), next

  | TEcall (id, e,l,_) -> let exp, fpmax = List.fold_right
                      (fun t (exp, fpmax) -> let e1, fpmax1 = alloc_expr env_alloc next t in
                                              e1::exp ,max fpmax fpmax1) e ([],next) in
                                              PEcall (id, exp, try Hashtbl.find function_env id with _ -> raise (ErrorCompiler(l, "undefined function" ^ id))), fpmax

  | TEblock (b,_,_) -> let t, fpmax = alloc_block env_alloc next b in
                PEblock(t), fpmax

and alloc_instruction env_alloc next ins =
  match ins with
  | TIexpr (e,_,_) -> let t, fpmax = (alloc_expr env_alloc next e) in PIexpr(t), fpmax, env_alloc

  | TIexAssign (id, e, _, _, _) -> let t, fpmax = (alloc_expr env_alloc next e) in
                            let te = extract_type_expr e in
                            let size = calculate_size te in
                            PImutExAssign (- next - 8, size, t), (max (next + size) fpmax),
                            Vmap.add id (- next - 8) env_alloc

  | TIstAssign (id1, id2, e1,_,l,_) -> (try (let st = Hashtbl.find representation_env id2 in
                                let st2 = Hashtbl.find struct_env id2 in
                                   let l_exp, fpmax = List.fold_right (fun (x,y) (e,fp) ->
                                      let t1,fp1 = alloc_expr env_alloc next y in
                                      let loc = List.assoc x st.fields in
                                      let size = List.assoc x st2 in
                                      (loc,size,t1)::e, max fp fp1) e1 ([],next) in
                                      PImutStAssign ( - next - 8, st.total, l_exp), (max fpmax (next + st.total)),
                                      Vmap.add id1 ( - next - 8) env_alloc)
                                      with _ -> raise (ErrorCompiler (l,"Instruction assign error")))

  | TIwhile (e, b,_,_) -> let e1, fpmax1 = (alloc_expr env_alloc next e) in
                       let b1, fpmax2 = (alloc_block env_alloc next b) in
                       PIwhile (e1,b1), (max fpmax1 fpmax2), env_alloc

  | TICreturn (f,e,_,_) -> let e1, fpmax = (alloc_expr env_alloc next e) in
                        PIreturn (f,e1), fpmax, env_alloc

  | TICreturnNull (f,_,_) -> PIreturnNull f, next, env_alloc

  | TIcond (c,_,_) -> let c1, fpmax1 = alloc_condition env_alloc next c in
                PIcond(c1), fpmax1, env_alloc


and alloc_block env_alloc next e =
  match e with
  | TCFullBlock (ins, e1,_,_) ->
    let ins1, size, env = List.fold_left (fun (x,y,z) exp ->
            let x1, y1, z1 = alloc_instruction z y exp in
            x1::x, max y y1, z1) ([],next,env_alloc) ins in
    let r, s = alloc_expr env size e1 in
    PCFullBlock (List.rev ins1, r), max size s

  | TCBlock (ins,_,_) ->
  let ins1, size, env = List.fold_left (fun (x,y,z) exp ->
          let x1, y1, z1 = alloc_instruction z y exp in
          x1::x, max y y1, z1) ([],next,env_alloc) ins in
          PCBlock(List.rev ins1), size

and alloc_condition env_alloc next c =
  match c with
  | TCif (e, bl1, None, _, _) -> let e1, fpmax1 = alloc_expr env_alloc next e in
                      let b1, fpmax2 = alloc_block env_alloc next bl1 in
                      PCif (e1, b1, None), max fpmax1 fpmax2
  | TCif (e, bl1, Some bl2, _, _) -> let e1, fpmax1 = alloc_expr env_alloc next e in
                      let b1, fpmax2 = alloc_block env_alloc next bl1 in
                      let b2, fpmax3 = alloc_block env_alloc next bl2 in
                      PCif (e1, b1, Some b2), max (max fpmax1 fpmax2) fpmax3
  | TCnestedIf (e ,b, i, _, _) -> let e1, fpmax1 = alloc_expr env_alloc next e in
                          let b1, fpmax2 = alloc_block env_alloc next b in
                          let i1, fpmax3 = alloc_condition env_alloc next i in
                          PCnestedIf (e1,b1,i1), max (max fpmax1 fpmax2) fpmax3

(*This function used to compile the general declaration
let empty_env = { evar = Hashtbl.create 17, level = 1 }*)

let alloc_decl d =
  match d with
  | TDecl_fun df -> let bl = df.body_tfunc in
                  let n = df.name_tfunc in
                  let ra = df.return_tfunc in
                  let arg = df.def_tfunc in
                  let env_alloc, next = List.fold_right (fun x (e,n) -> Vmap.add x.name_arg (n+calculate_size x.type_arg) e,
                                          (*(fprintf stdout "%d\n" (n + calculate_size x.type_arg);*)
                                          (n + calculate_size x.type_arg)) arg (Vmap.empty, 8) in
                  let b, fmax = alloc_block env_alloc 0 bl in
                  PDecl_fun ({ name_pfunc = n;
                    size_pfunc = next - 8;
                    return_pfunc = ra;
                    body_pfunc = b;}, fmax)
  | TDecl_struct ds -> Decl_struct ds

(*--------------------------------------------------------------------*)
(*Assembly code production*)

let popn n = addq (imm n) (reg rsp)

let pushn n = subq (imm n) (reg rsp)

let rec memmove octet =
  match octet with
  | i when i = 0 -> nop
  | i -> movq (ind ~ofs:(8-i) rcx) (reg rdx) ++ movq (reg rdx) (ind ~ofs:(8-i) rbx) ++ memmove (octet-8)

let rec memmove_topdown i octet =
  if i < octet then movq (ind ~ofs:(-i) rcx) (reg rdx) ++
                   movq (reg rdx) (ind ~ofs:(-i) rbx) ++
                   memmove_topdown (i+8) octet
  else nop

let rec compile_expr e =
  match e with
  | PEconst i -> movq (imm i) (reg rax) ++ pushq (reg rax)
  | PEbool b -> pushq (imm b)
  | PEvar (v,size) -> pushn size ++ leaq (ind ~ofs:(size-8) rsp) (rbx) ++ leaq (ind ~ofs:v rbp) (rcx) ++ memmove size
  | PEprint (s) ->  movq (ilab s) (reg rdi) ++ pushq (reg rax) ++ pushq (reg rbx)
                  ++ pushq (reg rcx) ++ pushq (reg rdx)
                  ++ movq (imm 0) (reg rax) ++ call "printf" ++
                  popq rdx ++ popq rcx++ popq rbx ++ popq rax
                  (*TODO:: save other register as well*)

  | PEbinop (o,exp1,exp2) -> compile_binop_expr o exp1 exp2
  | PEassign (exp1, exp2, size) -> compile_expr exp2 ++ compile_expr exp1 ++ popn size ++
                                  movq (reg rcx) (reg rbx) ++ leaq (ind ~ofs:(size-8) rsp) rcx ++
                                  memmove size ++ popn size
  | PEunop (o,e1) -> compile_unop_expr o e1
  | PEdereference (e1, size) -> compile_expr e1 ++ popq rax ++
                              pushn size ++ leaq (ind ~ofs:(size-8) rsp) rbx ++
                              leaq (ind ~ofs:0 rax) (rcx) ++ memmove size

  | PEreference (size,ex) -> compile_expr ex ++ popn size ++ leaq (ind ~ofs:0 rcx) rax ++ pushq (reg rax)

  | PEstruct (e1, size_st, offset, size_f, dr) ->
      let pre_code = compile_expr e1 ++
                    (if dr then popq rax ++ pushn size_st ++
                                leaq (ind ~ofs:(size_st - 8) rsp) (rbx) ++
                                leaq (ind ~ofs:0 rax) (rcx) ++ memmove size_st
                    else nop) in
      let copy_code = popn (size_st - size_f) ++ leaq (ind ~ofs:(-offset) rcx) rcx ++
                      leaq (ind ~ofs:(size_f - 8) rsp) (rbx) ++
                      memmove size_f in
      pre_code ++ copy_code

  | PEcall (id, exp, size) -> (*if(id = "print_int") then (fprintf stdout "size test%d\n" (List.length exp));*)
                              (try let fcall = List.fold_left (fun code e -> code ++ compile_expr e) nop exp in
                              pushn size ++ fcall ++ call id ++ popn (Hashtbl.find function_size_env id)
                              with _ -> raise (ShouldBeDealtByTC("Function Call Error" ^ id)))

  | PElength (exp,dr) -> let pre_code = if dr then
                          popq rcx ++ pushn 16 ++ leaq (ind ~ofs:8 rsp) rbx ++
                          memmove 16 else nop
                          in
                          compile_expr exp ++ pre_code ++ popn 8

  | PEindex (exp1, exp2, size, dr) -> let pre_code = if dr then
                                popq rcx ++ pushn 16 ++ leaq (ind ~ofs:8 rsp) rbx ++
                                memmove 16 else nop
                                in
                                compile_expr exp1 ++ pre_code ++ compile_expr exp2 ++
                                popq rax ++ movq (ind ~ofs:0 rsp) (reg rcx) ++ popn 16 ++
                                movq (imm size) (reg rdx) ++ imulq (reg rdx) (reg rax) ++
                                movq (imm (size - 8)) (reg rdx) ++ addq (reg rdx) (reg rax) ++
                                addq (reg rax) (reg rcx) ++ pushn size ++
                                leaq (ind ~ofs:(size - 8) rsp) rbx ++ memmove size


  | PEvector (size, size_field, exp) -> let code, i = List.fold_left (fun (x,y) ex ->
                                        x ++ (copy_vec ex size_field y), y + 1) (nop,0) exp in
                                        pushq (imm size) ++ movq (imm (size*size_field)) (reg rdi) ++
                                        pushq (reg rax) ++ movq (imm 0) (reg rax) ++ call "malloc" ++
                                        movq (reg rax) (reg rbx) ++ popq rax ++ pushq (reg rbx) ++ code

  | PEblock (bl) -> compile_block bl

and compile_ins ins =
  match ins with
  | PIexpr e -> compile_expr e
  | PImutExAssign (offset, size, e) -> compile_expr e ++ leaq (ind ~ofs:(size-8) rsp) (rcx) ++
                                      leaq (ind ~ofs:offset rbp) (rbx) ++
                                      memmove size ++ popn size
  | PIexAssign (offset, size, e) -> compile_expr e ++ leaq (ind ~ofs:(size-8) rsp) (rcx) ++
                                      leaq (ind ~ofs:offset rbp) (rbx) ++
                                      memmove size ++ popn size
  | PImutStAssign (offset, size, list_exp) -> List.fold_left (fun code ex -> code ++ compile_field_init ex offset) nop list_exp
  | PIstAssign (offset, size, list_exp) -> List.fold_left (fun code ex -> code ++ compile_field_init ex offset) nop list_exp
  | PIwhile (e,b) -> let label1 = find_next_global 0 2 in
                    let label2 = find_next_global 0 2 in
                    (label label1) ++ compile_expr e ++ popq rax ++
                    cmpq (imm 0) (reg rax) ++ je label2 ++
                    compile_block b ++ jmp label1 ++ (label label2)
  | PIcond c -> compile_condition c
  | PIreturn (f,e) -> (try let return_label = Hashtbl.find return_label_env f in compile_expr e ++ jmp return_label
                      with _ -> raise (ShouldBeDealtByTC("Function Return Error" ^ f)))
  | PIreturnNull f -> (try let return_label = Hashtbl.find return_label_env f in
                      jmp return_label
                      with _ -> raise (ShouldBeDealtByTC("Function Return Error" ^ f)))
  | _ -> nop

and compile_condition c =
  match c with
  | PCif (e, block1, None) -> (*print_string "shit, found you\n";*)
                              let label1 = find_next_global 0 1 in
                              let t1 = compile_expr e in
                              let b1 = compile_block block1 in
                              t1 ++ popq rax ++ cmpq (imm 0) (reg rax) ++ je label1 ++
                              b1 ++ (label label1)
  | PCif (e, block1, Some block2) -> let label1 = find_next_global 0 1 in
                              let label2 = find_next_global 0 1 in
                              let t1 = compile_expr e in
                              let b1 = compile_block block1 in
                              let b2 = compile_block block2 in
                              t1 ++ popq rax ++ cmpq (imm 0) (reg rax) ++ je label1 ++
                              b1 ++ jmp label2 ++ (label label1) ++ b2 ++ (label label2)
  | PCnestedIf (e, block1, cond) -> let label1 = find_next_global 0 1 in
                              let label2 = find_next_global 0 1 in
                              let t1 = compile_expr e in
                              let b1 = compile_block block1 in
                              let b2 = compile_condition cond in
                              t1 ++ popq rax ++ cmpq (imm 0) (reg rax) ++ je label1 ++
                              b1 ++ jmp label2 ++ (label label1) ++ b2 ++ (label label2)

and compile_block bl =
  (match bl with
  | PCFullBlock (pins, e1) -> (List.fold_left (fun x y -> let t = compile_ins y in x ++ t) nop pins) ++ (compile_expr e1)
  | PCBlock pins -> List.fold_left (fun x y -> let t = compile_ins y in x ++ t) nop pins)

and compile_binop_expr op e1 e2 =
  let t1 = compile_expr e1 in
  let t2 = compile_expr e2 in
  match op with
  | Badd | Bsub | Bmul -> compile_binop_simple_arithmetic op t1 t2
  | Band | Bor -> compile_binop_lazy_simple_arithmetic op t1 t2
  | Bdiv | Bmod -> compile_binop_complex_arithmetic op t1 t2
  | Beq | Bneq | Blt | Ble | Bgt | Bge -> compile_binop_condition op t1 t2

and compile_binop_simple_arithmetic op t1 t2 =
  let o = match op with
  | Badd -> addq
  | Bsub -> subq
  | Bmul -> imulq
  in t1 ++ t2 ++ popq rdx ++ popq rax ++
  o (reg rdx) (reg rax) ++ pushq (reg rax)

and compile_binop_lazy_simple_arithmetic op t1 t2 =
  let label1 = find_next_global 0 3 in
  match op with
  | Band -> t1 ++ movq (ind ~ofs:0 rsp) (reg rax) ++ cmpq (imm 0) (reg rax) ++
            je label1 ++ popq rax ++ t2 ++ (label label1)
  | Bor -> t1 ++ movq (ind ~ofs:0 rsp) (reg rax) ++ cmpq (imm 1) (reg rax) ++
            je label1 ++ popq rax ++ t2 ++ (label label1)

and compile_binop_condition op t1 t2 =
  let o = match op with
  | Beq -> sete
  | Bneq -> setne
  | Blt -> setl
  | Ble -> setle
  | Bgt -> setg
  | Bge -> setge
  in
  t1 ++ t2 ++ popq rdx ++ popq rax ++
  cmpq (reg rdx) (reg rax) ++
  o (reg al) ++ movzbq (reg al) rax ++
  pushq (reg rax)

and compile_binop_complex_arithmetic op t1 t2 =
  let d = match op with
  | Bdiv -> reg rax
  | Bmod -> reg rdx
  in t1 ++ t2 ++ popq rbx ++ popq rax ++
  cqto ++ idivq (reg rbx) ++ pushq d


and compile_unop_expr o e =
  match o with
  | Uneg -> compile_expr e ++ popq rax ++ negq (reg rax) ++ pushq (reg rax)
  | Unot -> compile_expr e ++ popq rax ++ movq (imm 1) (reg rdx) ++ xorq (reg rdx) (reg rax) ++ pushq (reg rax)


and compile_field_init e gofs =
  let offset, size, e1 = e in
  compile_expr e1 ++ leaq (ind ~ofs:(gofs-offset) rbp) (rbx) ++
  leaq (ind ~ofs:(size-8) rsp) (rcx) ++ memmove size ++
  popn size

and copy_vec e size_field index =
  compile_expr e ++ movq (ind ~ofs:size_field rsp) (reg rbx) ++
  addq (imm (index*size_field + size_field - 8)) (reg rbx) ++
  leaq (ind ~ofs:(size_field - 8) rsp) rcx ++ memmove size_field ++
  popn size_field

let compile_decl d =
  match d with
  | PDecl_fun (func, fpmax) -> ((*try*)
    let size_return = try Hashtbl.find function_env func.name_pfunc with _ -> raise (ShouldBeDealtByTC("Function Return Size Error")) in

    let return_address = 8 + (Hashtbl.find function_size_env func.name_pfunc) + size_return in

    let return_label = Hashtbl.find return_label_env func.name_pfunc in

    let code = (label func.name_pfunc) ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    pushn fpmax ++
    compile_block func.body_pfunc ++
    (label return_label) ++
    leaq (ind ~ofs:(size_return-8) rsp) (rcx) ++
    leaq (ind ~ofs:return_address rbp) (rbx) ++ memmove size_return ++
    leave ++
    ret in
    if func.name_pfunc = "main" then code, nop
    else nop, code
    (*with _ -> raise (Stack_overflow "8")*))
  | Decl_struct s -> nop, nop

let compile_program p ofile =
  calculate_rep p;
  let p1 = List.map alloc_decl p in
  let codemain, codefun = nop, nop in
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
