open Format
open X86_64
open Ast
open Ast_code
open Tc

exception VarUndef of string

let ref frame_size = 0

type structure = { total: int;
                   fields: (string * int) list}

let ref chaine = 0
(*the total size of *)

module Vmap = Map.Make(String)

type alloc = int Vmap.t

let representation_env = (Hashtbl.create 20 : (string,structure) Hashtbl.t);;

let struct_env = (Hashtbl.create 20 : (string,unit) Hashtbl.t);;

let function_env = (Hashtbl.create 20 : (string,unit) Hashtbl.t);;

let variable_env = (Hashtbl.create 20 : (string,string) Hashtbl.t);;


(*Calculate the size of each type*)

let rec calculate_size typ =
  match typ with
  | Tnull | Tint | Tbool -> 8
  | Tstruct id -> let t = Hashtbl.find representation_env id in t.total
  | Tstructgeneric _ -> 16
  | Tref t -> 8
  | Tmut t -> calculate_size t

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
    | Decl_fun df -> ()
    | Decl_struct ds -> let name = ds.name_struct in
                      let field_list = ds.def_struct in
                      let total = List.fold_left (fun x y -> x + calculate_size y.type_struct_arg) 0 field_list in
                      let fields = List.fold_right (fun y x -> (y.name_struct_arg, calculate_size y.type_struct_arg)::x) field_list [] in
                      Hashtbl.add representation_env name {total = total; fields = (calculate_relative_position fields 0)}
  in List.iter calculate_struct p

(*This function is dedicated for the declaration of function*)

let rec alloc_expr env_alloc env_typ next e =
  match e with
  | Econst i -> PEconst i, next

  | Ebool i when i = true -> PEbool 1, next

  | Ebool i when i = false -> PEbool 0, next

  | Evar x ->
    begin
    try
      let ofs_x = Smap.find x env_alloc in
      PEvar ofs_x, next
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
  (*| Estruct of expr * ident
  | Elength of expr
  | Eindex of expr * expr*)

  | Eprint s -> Hashtbl.add variable_env s s ; PEprint(s), next

  | Ecall (id, e) -> let exp, fpmax = List.fold_right
                      (fun t (exp, fpmax) -> let e1, fpmax1 = alloc_expr env_alloc env_typ next t
                                              in e1::exp ,max fpmax fpmax1) e ([],next)
                                              in PEcall (id, List.rev exp), fpmax
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
                            PImutExAssign (- next - size, t), (max (next + size) fpmax), Smap.add id (- next - size) env_alloc

  (*| ImutStAssign of id1 * id2 * ((ident * expr) list ) -> PImutStAssign of int * ident * ((ident * pexpr) list ) * int
  | IexAssign id * e -> let t, fpmax = (alloc_expr env next e) in
                                    PImutExAssign (- next - 8, t), (max (next + 8), fpmax), Smap.add id (- next - 8) env
  | IstAssign of ident * ident * ((ident * expr) list ) -> PIstAssign of int * ident * ((ident * pexpr) list ) * int
  | Iwhile of e * b -> let e1, fpmax1 = (alloc_expr env next e) in
                       let b1, fpmax2 = (alloc_block env next b) in
                       PIwhile (e1,b1), (max fpmax1 fpmax2), env
  | Ireturn of expr -> let e1, fpmax = (alloc_expr env next e) in
                        PIreturn (e1), fpmax, env
  | IreturnNull -> PIreturnNull , next, env
  | Icond c -> let c1, fpmax1 = alloc_condition env next c in
                PIcond(c1), fpmax1, env*)

  | _ -> raise (VarUndef("vvvv"))


and alloc_block env_alloc env_type next e =
  match e with
  | CFullBlock (ins, e1) ->
    let ins1, size, env = List.fold_left (fun (x,y,z) exp ->
            let x1, y1, z1 = alloc_instruction z env_type y exp in
            x1::x, max y y1, z1) ([],next,env_alloc) ins in
    let x, y = alloc_expr env env_type size e1 in
    PCFullBlock (ins1, x), max size y

  | CBlock ins -> let ins1, size, env = List.fold_left (fun (x,y,z) exp ->
          let x1, y1, z1 = alloc_instruction z env_type y exp in
          x1::x, max y y1, z1) ([],next,env_alloc) ins in
          PCBlock(ins1), size

(*and alloc_condition env_alloc env_type next c =
  match c with
  | Cif (e, b1, b2) -> let e1, fpmax1 = alloc_expr env next e in
                      let b1, fpmax2 = alloc_block env next b1 in
                      let b2, fpmax3 = alloc_block env next b2 in
                      PCif (e1, b1, b2), max (max fpmax1 fpmax2) fpmax3
  | CnestedIf (e ,b, i) -> let e1, fpmax1 = alloc_expr env next e in
                          let b1, fpmax2 = alloc_block env next b in
                          let i1, fpmax3 = alloc_condition env next i in
                          PCnestedIf (e1,b1,i1), max (max fpmax1 fpmax2) fpmax3*)

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
                  let env_alloc, next = List.fold_right (fun x (e,n) -> Vmap.add x.name_arg (calculate_size x.type_arg) e,
                                          n + calculate_size x.type_arg) arg (Vmap.empty, 8) in
                  let b, fmax = alloc_block env_alloc env_type 0 bl in
                  PDecl_fun ({ name_func = n;
                    return_func = ra;
                    body_func = b;}, fmax)
  | Decl_struct ds -> Decl_struct ds

(*let int_expr exp =
  let rec comprec env next
    | Econst n -> movq (imm n) (reg rdi)
    | Ebool b -> let temp = if b then 1 else 0 in movq (imm temp) (reg rdi)
    | Ebinop*)

let compile_program p ofile =
  calculate_rep p;
