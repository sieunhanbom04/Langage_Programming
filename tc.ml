
open Ast
open Printf

module Smap = Map.Make(String)

type status = Vide | Plein | Borrow | Borrowmut

type env_var = (string, (typ * bool * status)) Hashtbl.t (* The boolean indicates whether the variable is mutable *)
type env_struct = (string, (struct_argument list)) Hashtbl.t
type func = {args : argument list; return : typ}
type env_func = (string, func) Hashtbl.t

(*let genv_struct : env_struct = Smap.empty
let genv_func : env_func = Smap.empty
*)

let genv_struct = Hashtbl.create 17 (*global environment for structures*)
let genv_func = Hashtbl.create 17 (*global enviroment for functions*)

type env = {mutable evar : env_var; mutable level : int} (*local environment when typechecking a function )*)

exception Type_Error of string

let rec str_type = function (*a function that turns a type into a string, mainly for debugging*)
 	| Tnull -> " ()"
	| Tint -> " i32"
	| Tbool -> " bool"
	| Tstruct id  ->  " id "
	| Tstructgeneric (id, t) -> "Vec<"^ (str_type t) ^">"
  | Tref t -> "&"^ (str_type t)
  | Tmut t -> " mut "^ (str_type t)

let print_type t = print_string (str_type t) (*a function that prints types*)

(*
type typ = Tnull
          | Tint
          | Tbool
          | Tstruct of ident
          | Tvec of typ
          | Tref of typ
          | Tmut of typ

*)


(*let rec change larg =
	match larg with
	| [] -> []
	| x :: rest -> (x.name,x.types)::(change rest)*)

let rec usable_as t1 t2 = if t1 = t2 then true else match (t1, t2) with (*predicate on whether a variable of type t1 can be used as t2*)
	|Tref t , Tref t' -> usable_as t t'
	|Tmut t , t' -> usable_as t t'
	|Tstructgeneric (_,t) , Tstructgeneric (_,t')  -> usable_as t t'
	| _ -> false

let rec type_check_instr env = function (*type checking for instructions*)

  | Inothing -> Tnull
  | Iexpr e -> type_check_expr env e

  | ImutExAssign (id, e) -> let te = type_check_expr env e in Hashtbl.add env.evar id (te, true, Plein); print_endline ("tc : adding variable " ^ id) ; Tnull

  | ImutStAssign (idv, idst, lid) -> if not (Hashtbl.mem genv_struct idst) then raise (Type_Error ("Undeclared structure " ^ idst));
										let larg = Hashtbl.find genv_struct idst in
										if not (List.for_all2 (fun x y -> x.name_struct_arg = fst y && x.type_struct_arg = (type_check_expr env (snd y))) larg lid) then raise (Type_Error "Incoherent structure assignment");
											Hashtbl.add env.evar idv (Tstruct idst,true ,Plein);
											Tnull

  | IexAssign (id, e) -> let te = type_check_expr env e in Hashtbl.add env.evar id (te, false, Plein); Tnull
  | IstAssign (idv, idst, lid) -> if not (Hashtbl.mem genv_struct idst) then raise (Type_Error ("Undeclared structure " ^ idst));
										let larg = Hashtbl.find genv_struct idst in
										if not (List.for_all2 (fun x y -> x.name_struct_arg = fst y && x.type_struct_arg = (type_check_expr env (snd y))) larg lid) then raise (Type_Error "Incoherent structure assignment");
										 Hashtbl.add env.evar idv (Tstruct idst,false ,Plein);
											Tnull

  | Iwhile (expr, bl) -> type_check_condition env expr; type_check_block env bl ; Tnull
  | Ireturn e -> type_check_expr env e
  | IreturnNull -> Tnull
  | Icond cond -> type_check_if env cond

and type_check_expr env = function

  | Econst _ -> Tint
  | Ebool _ -> Tbool
  | Evar id -> if (Hashtbl.mem env.evar id) then let xx, _, _ = (Hashtbl.find env.evar id) in xx else raise (Type_Error ("Unbound variable " ^ id))
  | Ebinop (b, e1, e2) -> type_check_binop env b e1 e2
  | Eunop (u, e) -> type_check_unop env u e
  | Estruct (e, id) -> (match type_check_expr env e with
																								|Tstruct st -> let stl = Hashtbl.find genv_struct st in
																								try let elem = (List.find (fun xx -> xx.name_struct_arg = id) stl) in elem.type_struct_arg with Not_found -> raise (Type_Error "Unknown structure field")
																								| _ -> raise (Type_Error "Field indexing is only possible for structures"))  (*********************************************)


  | Elength e -> (match (type_check_expr env e) with
									|Tstructgeneric _ | Tref Tmut Tstructgeneric _ | Tref Tstructgeneric _ -> Tint
									| _ -> raise (Type_Error "length method can only be used on vectors") )
  | Eindex (v, e) -> if (type_check_expr env e) <> Tint then raise (Type_Error "Index should be integer");
										(let tv = (type_check_expr env v) in match tv with
											|Tstructgeneric (_, t) |Tmut Tstructgeneric (_, t) |Tref Tstructgeneric (_, t) |Tref Tmut Tstructgeneric (_, t) -> t
											| _ -> print_type tv; raise (Type_Error ("Unbound vector identifier or indexing of non vector variable")))

	| Ecall (id, l) -> let f = try Hashtbl.find genv_func id with Not_found -> raise (Type_Error ("Undeclared function " ^ id)) in
											let lt = List.map (type_check_expr env) l in
											let lty  = List.map (fun a -> a.type_arg) f.args in
											(try List.iter2 (fun a b -> if not (usable_as a b) then raise (Type_Error ("Incorrect argument type in function " ^ id ^ " " ^ (str_type a) ^ " not usable as "^(str_type b)))) lt lty with (Invalid_argument "List.iter2") -> raise (Type_Error ("Too many/few arguments for function "^id)))
											; f.return

  | Eprint s -> Tnull
  | Evector l -> if List.length l > 0 then let lt = List.map (type_check_expr env) l in
													if not (List.for_all (fun x -> x = List.hd lt) lt) then raise (Type_Error "Non homogenous vector") else Tstructgeneric ("vec", List.hd lt) else Tstructgeneric ("vec",Tnull) (************************************* *)
  | Eblock bl -> type_check_block env bl

and type_check_block env = function
  | CFullBlock (l, exp) -> let lt = List.map (type_check_instr env) l in type_check_expr env exp
  | CBlock l -> let tl = List.rev (List.map (type_check_instr env) l) in try List.hd tl with Failure "hd" -> Tnull



and type_check_if env = function
  | Cif (e, b1, b2) -> type_check_condition env e; let tb1 = type_check_block env b1 in
																										let tb2 = type_check_block env b2 in
																										if tb1 <> tb2 then raise (Type_Error "different types in if alternatives") else tb1
  | CnestedIf (e, bl, c) -> type_check_condition env e; let tbl = type_check_block env bl in
																												let tc = type_check_if env c in
																												if tbl <> tc then raise (Type_Error "different types in if alternatives") else tbl

and type_check_condition env c = if (type_check_expr env c) <> Tbool then raise (Type_Error "condition should be boolean")

and type_check_unop env u e = let te = (type_check_expr env e) in match u with
  | Uneg when te = Tint -> Tint (* -e *)
  | Unot when te = Tbool -> Tbool(* not e *)
  | Unstar -> (match te with | Tref ty -> ty | _ -> raise (Type_Error "Dereferencing operator applied to non reference value") )
  | Unp when is_lvalue env e  -> Tref te
  | Unmutp when is_lvalue env e && is_mutable env e -> Tref (Tmut te) (************************************* *)
  | _ -> raise (Type_Error "Incorrect unary operator")

and type_check_binop env b e1 e2 =
  let te1 = type_check_expr env e1 in
  let te2 = type_check_expr env e2 in
	let nonint = te1 <> Tint || te2 <> Tint in
	match b with
  | Badd | Bsub | Bmul | Bdiv | Bmod -> if nonint then raise (Type_Error "Non integer in arithmetic expression"); Tint
  | Beq | Bneq | Blt | Ble | Bgt | Bge -> if nonint then raise (Type_Error "Comparison with non integer"); Tbool (* == != < <= > >= *)
  | Band | Bor -> if te1 <> Tbool || te2 <> Tbool then
		  raise (Type_Error "Logical operator should be applied to bool values"); Tbool
	|Bassign -> if not (is_lvalue env e1) then raise (Type_Error "Assigning to non left value");
							if not (is_mutable env e1) then raise (Type_Error "Assigning to non mutable variable");
							if not (usable_as te2 te1) then raise (Type_Error ("Assigning to incompatible type "^(str_type te2)^" -> "^ (str_type te1))); Tnull

and is_mutable env exp = match (type_check_expr env exp) with |Tmut _ -> true | _ -> (match exp with
	| Evar idd -> let _,m,_ = Hashtbl.find env.evar idd in m
	| Estruct (st, id) -> is_mutable env st
	| Eindex (v, e) -> is_mutable env v
  | Eunop (Unstar, e) -> (match (type_check_expr env e) with |Tref Tmut _ -> true | _ -> false)
	| _ -> false)
(***************************************)

and is_lvalue env = function
	| Evar id when Hashtbl.mem env.evar id && (let _,m,_ = Hashtbl.find env.evar id in m) -> true
	| Eindex (v, e) -> is_mutable env v || (let tv = (type_check_expr env v) in match tv with
											|Tref Tstructgeneric (_, t) |Tref Tmut Tstructgeneric (_, t) -> true |_ -> false)
	| Estruct (e, id) -> is_mutable env e
	| _ -> false

let type_check_fndecl df = let env = {evar = Hashtbl.create 17; level = 1} in
List.iter (fun x -> Hashtbl.add env.evar x.name_arg (x.type_arg, x.mut_arg, Plein)) df.def_func	;
let tb = type_check_block env df.body_func in
if tb <> df.return_func then raise (Type_Error "Incorrect return type for function ") ; Hashtbl.add genv_func df.name_func {args=df.def_func; return = df.return_func} ; print_string ("function " ^ df.name_func ^ "(" ); List.iter (fun a -> print_type a.type_arg) df.def_func;print_endline ")"



let rec check_welltyped = function
	| Tnull | Tint | Tbool -> true
	| Tref t -> check_welltyped t
	| Tmut t -> check_welltyped t
	| Tstruct id -> Hashtbl.mem genv_struct id
	| Tstructgeneric (_, t) -> check_welltyped t
	| _ -> false

let type_check_decl decl =  match decl with
	|Decl_fun df -> type_check_fndecl df
	|Decl_struct ds -> if not (List.for_all (fun x -> check_welltyped x.type_struct_arg) ds.def_struct) then raise (Type_Error "Incorrectly typed field in structure declaration");
														Hashtbl.add genv_struct ds.name_struct ds.def_struct

let gen_type_check p = print_int (List.length p); print_endline " declarations found, proceeding to type checking";List.iter type_check_decl p
(*take a program and check their function*)
