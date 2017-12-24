
open Ast
open Printf

module Sset = Set.Make(String)


type status = Vide | Plein | Borrow | Borrowmut

type env_var = (string, (typ * bool * status)) Hashtbl.t (* The boolean indicates whether the variable is mutable *)
type env_struct = (string, (struct_argument list)) Hashtbl.t
type func = {args : argument list; return : typ}
type env_func = (string, func) Hashtbl.t


let genv_struct = Hashtbl.create 17 (*global environment for structures*)
let genv_func : env_func = Hashtbl.create 17 (*global environment for functions*)


type env = {mutable evar : env_var; mutable level : int; mutable ref_lifespans : (string, int) Hashtbl.t} (*local environment when typechecking a function )*)

exception Type_Error of string

let rec below ty = match ty with |Tref t -> below t | Tmut t -> below t | _ -> ty

let is_move = function 
	| Tref Tmut _ | Tstructgeneric _ | Tstruct _ -> true
	|_ -> false


let check_and_delete_ownership env e = match e with 
	|Evar v -> let tv,mv,stat = Hashtbl.find env.evar v in 
													if stat = Vide then raise (Type_Error ("Access to void variable : " ^ v)); 
													if is_move tv then Hashtbl.replace env.evar v (tv, mv, Vide) 
	| _ -> ()

let rec str_type = function (*a function that turns a type into a string, mainly for debugging*)
 	|Tnull -> " ()"
	| Tint -> " i32"
	| Tbool -> " bool"
	| Tstruct id  ->  id
	| Tstructgeneric (id, t) -> "Vec<"^ (str_type t) ^">"
  | Tref t -> "&"^ (str_type t)
  | Tmut t -> " mut "^ (str_type t)

let print_type t = print_string (str_type t) (*a function that prints types*)


let rec check_noborrow = function 
	|Tnull | Tint | Tbool |Tstruct _ -> true (* Structure fields are guaranteed not to contain borrows*)
	|Tstructgeneric (_, t) -> check_noborrow t
	|Tmut t -> check_noborrow t
	|Tref _ -> false

let check_ownership env v = match v with 
	|Evar w -> let _, _, stat = Hashtbl.find env.evar w in if stat = Vide then raise (Type_Error ("Access to void variable : " ^ w)) 
	| _ -> ()


let rec usable_as t1 t2 = if t1 = t2 then true else match (t1, t2) with (*predicate on whether a variable of type t1 can be used as t2*)
	|Tref t , Tref t' -> usable_as t t' 
	|Tmut t , Tmut t' -> usable_as t t' 
	|Tmut t , t' -> usable_as t t'
	|t, Tmut t' when not (is_move t) -> usable_as t t'
	|Tstructgeneric (_,t) , Tstructgeneric (_,t')  -> usable_as t t' 
	| _ -> false

let rec assignable t1 t2 = if t1 = t2 then true else match (t1, t2) with (*predicate on whether a variable's value of type t1 can be assigned to a variable of type t2*)
	|t , Tmut t' -> assignable t t' 
	| _ -> usable_as t1 t2


let rec type_check_instr env = function (*type checking for instructions*)

  | Inothing -> Tnull
  | Iexpr e -> type_check_expr env e
  | ImutExAssign (id, e) -> let te = type_check_expr env e in Hashtbl.add env.evar id ((match te with Tmut _ -> te |_ -> Tmut te), true, Plein)(*; print_endline ("tc : adding variable " ^ id) *); 
														check_and_delete_ownership env e; 
												begin match e with 
												|Eunop (Unmutp, Evar var) -> check_ownership env (Evar var); let a,b,stat = Hashtbl.find env.evar var in if stat = Borrowmut then raise (Type_Error "Only one mutable Borrow can be made of a variable") ; if not (is_mutable env (Evar var)) then raise (Type_Error "Cannot mutably borrow a non mutable variable");
												Hashtbl.replace env.evar var (a, b, Borrowmut);
												Hashtbl.add env.ref_lifespans id env.level
												|Eunop (Unp, Evar var) -> check_ownership env (Evar var);let a,b,stat = Hashtbl.find env.evar var in Hashtbl.replace env.evar var (a, b, if stat = Borrowmut then stat else Borrow);
												Hashtbl.add env.ref_lifespans id env.level
												|_ -> ()
												end
								; Tnull

  | ImutStAssign (idv, idst, lid) -> if not (Hashtbl.mem genv_struct idst) then raise (Type_Error ("Undeclared structure " ^ idst));
										let larg = Hashtbl.find genv_struct idst in
										check_struct_fields env larg lid;
										List.iter (fun x -> check_and_delete_ownership env (snd x)) lid;
											Hashtbl.add env.evar idv (Tmut (Tstruct idst),true ,Plein);
											Tnull

  | IexAssign (id, e) -> let te = type_check_expr env e in Hashtbl.add env.evar id (te, false, Plein);
														check_and_delete_ownership env e; 
												begin match e with 
												|Eunop (Unmutp, Evar var) -> let a,b,stat = Hashtbl.find env.evar var in 
												if stat = Borrowmut then raise (Type_Error "Only one mutable Borrow can be made of a variable"); 
												if stat = Vide then raise (Type_Error "Cannot borrow a void variable"); 
												Hashtbl.replace env.evar var (a, b, Borrowmut);
												Hashtbl.replace env.ref_lifespans id env.level
												|Eunop (Unp, Evar var) -> let a,b,stat = Hashtbl.find env.evar var in Hashtbl.replace env.evar var (a, b, if stat = Borrowmut then stat else Borrow);
													Hashtbl.replace env.ref_lifespans id env.level
												|_ -> ()
												end
								; Tnull


  | IstAssign (idv, idst, lid) -> if not (Hashtbl.mem genv_struct idst) then raise (Type_Error ("Undeclared structure " ^ idst));
										let larg = Hashtbl.find genv_struct idst in
										check_struct_fields env larg lid;
										List.iter (fun x -> check_and_delete_ownership env (snd x)) lid;
										 Hashtbl.add env.evar idv (Tstruct idst,false ,Plein);
											Tnull

  | Iwhile (expr, bl) -> type_check_condition env expr; type_check_block env bl ; Tnull
  | ICreturn (f,e) -> let te = type_check_expr env e in
											let df = Hashtbl.find genv_func f in 
											if not (usable_as te df.return) then raise (Type_Error "Incorrect return type for function");
											te
  | ICreturnNull f -> let df = Hashtbl.find genv_func f in if not (usable_as Tnull df.return) then raise (Type_Error "Incorrect return type for function"); Tnull 
  | Icond cond -> type_check_if env cond
	| _ -> Tnull (***This will correspond to Ireturn and IreturnNull but they shouldn't occur***)

and type_check_expr env = function

  | Econst _ -> Tint
  | Ebool _ -> Tbool
  | Evar id -> if (Hashtbl.mem env.evar id) then let xx, _, _ = (Hashtbl.find env.evar id) in xx else raise (Type_Error ("Unbound variable " ^ id))
  | Ebinop (b, e1, e2) -> type_check_binop env b e1 e2
  | Eunop (u, e) -> type_check_unop env u e
  | Estruct (e, id) -> check_and_delete_ownership env e ; (match below (type_check_expr env e) with 
																								|Tstruct st -> let stl = Hashtbl.find genv_struct st in
																								try let elem = (List.find (fun xx -> xx.name_struct_arg = id) stl) in elem.type_struct_arg with Not_found -> raise (Type_Error "Unknown structure field")
																								| _ -> raise (Type_Error "Field indexing is only possible for structures"))  (*********************************************)


  | Elength e -> (match (type_check_expr env e) with 
									|Tstructgeneric _ | Tref Tmut Tstructgeneric _ | Tref Tstructgeneric _ -> Tint
									| _ -> raise (Type_Error "length method can only be used on vectors") )
  | Eindex (v, e) -> if (type_check_expr env e) <> Tint then raise (Type_Error "Index should be integer");
										check_ownership env v;
										(let tv = (type_check_expr env v) in match tv with 
											|Tstructgeneric (_, t) |Tmut Tstructgeneric (_, t) |Tref Tstructgeneric (_, t) |Tref Tmut Tstructgeneric (_, t) -> t
											| _ -> print_type tv; raise (Type_Error ("Unbound vector identifier or indexing of non vector variable")))
		     
	| Ecall (id, l) -> let f = try Hashtbl.find genv_func id with Not_found -> raise (Type_Error ("Undeclared function " ^ id)) in 
											let lt = List.map (type_check_expr env) l in
											let lty  = List.map (fun ar -> ar.type_arg) f.args in
											let () = List.iter (fun x -> check_and_delete_ownership env x) l in 
											(try List.iter2 (fun a b -> if not (usable_as a b) then raise (Type_Error ("Incorrect argument type in function " ^ id ^ " " ^ (str_type a) ^ " not usable as "^(str_type b)))) lt lty with (Invalid_argument "List.iter2") -> raise (Type_Error ("Too many/few arguments for function "^id)))
											; f.return

  | Eprint s -> Tnull
  | Evector l -> if List.length l = 0 then Tstructgeneric ("vec",Tnull) else
								 let lt = List.map (type_check_expr env) l in
									List.iter2 (fun x y -> ( match x with | Evar v -> if is_move y then let tv, mv, statv = Hashtbl.find env.evar v in if statv <> Plein then raise (Type_Error "Cannot move a non owning variable in vector declaration") else Hashtbl.replace env.evar v (tv, mv, Vide) | _ -> ()) ) l lt;
																let tgen = try List.find (fun x -> List.for_all (fun y -> usable_as x y) lt) lt with Not_found -> raise (Type_Error "Non homogenous vector") in Tstructgeneric ("vec", tgen) 
																(************************************* *)
  | Eblock bl -> type_check_block env bl

and type_check_block env = function
  | CFullBlock (l, exp) -> env.level <- env.level +1; 
														List.iter (fun x -> type_check_instr env x; () ) l;
														let tt = type_check_expr env exp in env.level <- env.level -1;
								 tt
  | CBlock l -> env.level <- env.level +1; 
								let tl = List.rev (List.map (type_check_instr env) l) in 
								env.level <- env.level -1;
								try List.hd tl with Failure "hd" -> Tnull



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
  | Unstar -> let tr = (match te with | Tref ty -> ty | Tmut Tref ty -> ty | _ -> raise (Type_Error "Dereferencing operator applied to non reference value") ) in
							(match e with Evar v -> if env.level < (Hashtbl.find env.ref_lifespans v) then raise (Type_Error "Use of an expired reference") |_ -> ()); tr
  | Unp (*when is_lvalue env e*) (**See test borrow 4***)  -> (match e with Evar _ -> () | _ -> raise (Type_Error "Reference cannot be created for non variables")); Tref te 
  | Unmutp -> if not (is_lvalue env e) then raise (Type_Error "References can only be created for leftvalues");
							if not (is_mutable env e) then raise (Type_Error "Mutable references can only be created for mutable values");
							Tref (match te with Tmut _ -> te |_-> Tmut te) (************************************* *)
  | _ -> raise (Type_Error "Incorrect unary operator ") 

and type_check_binop env b e1 e2 = 
  let te1 = type_check_expr env e1 in
  let te2 = type_check_expr env e2 in
	let nonint = not (usable_as te1 Tint && usable_as te2 Tint) in
	match b with
  | Badd | Bsub | Bmul | Bdiv | Bmod -> if nonint then raise (Type_Error "Non integer in arithmetic expression"); Tint
  | Beq | Bneq | Blt | Ble | Bgt | Bge -> if nonint then raise (Type_Error "Comparison with non integer"); Tbool (* == != < <= > >= *)
  | Band | Bor -> if te1 <> Tbool || te2 <> Tbool then 
		  raise (Type_Error "Logical operator should be applied to bool values"); Tbool
	|Bassign -> let me1 = is_mutable env e1 in
							if not (is_lvalue env e1) then raise (Type_Error "Assigning to non left value");
							if not (me1) then raise (Type_Error "Assigning to non mutable variable");
							if not (assignable te2 te1) then raise (Type_Error ("Assigning to incompatible type "^(str_type te2)^" -> "^ (str_type te1))); 
							(match e2 with 
										|Eunop(Unmutp, (Evar v)) -> let tv, mv, statv = Hashtbl.find env.evar v in 
																								if statv = Borrowmut then raise (Type_Error "Only one mutable Borrow can be made of a variable");
																								if statv = Vide then raise (Type_Error "Cannot borrow a void variable");
																								Hashtbl.replace env.evar v (tv, mv, Borrowmut);
																								(match e1 with Evar w -> Hashtbl.replace env.ref_lifespans w env.level |_ -> ())

										|Eunop(Unp, (Evar v)) -> let tv, mv, statv = Hashtbl.find env.evar v in 
																							if statv = Vide then raise (Type_Error "Cannot borrow a void variable");
																	
																							Hashtbl.replace env.evar v (tv, mv, (if statv = Borrowmut then statv else Borrow));
																							
																							(match e1 with Evar w -> Hashtbl.replace env.ref_lifespans w env.level |_ -> ())
	
										|Evar v  ->	let tv, mv, statv = Hashtbl.find env.evar v in 
																if statv <> Plein then raise (Type_Error "Cannot assign the content of a non owning variable");
																Hashtbl.replace env.evar v (tv, mv, Vide);
										| _ -> ()
													(**Should we manage assignments to vector cells ???**)										

							);
							(match e1 with 
									|Evar w -> let tw, mw, statw = Hashtbl.find env.evar w in if (statw = Borrow || statw = Borrowmut) then raise (Type_Error "Cannot modify a borrowed variable");
									Hashtbl.replace env.evar w (tw, mw, Plein)
							| _ -> ()
);
Tnull 

and is_mutable env exp = match (type_check_expr env exp) with |Tmut _ | Tref Tmut Tstructgeneric _ -> true | _ -> (match exp with
	| Evar idd -> let t,m,_ = Hashtbl.find env.evar idd in m
	| Estruct (st, id) -> is_mutable env st
	| Eindex (v, e) -> is_mutable env v
  | Eunop (Unstar, e) -> (match (type_check_expr env e) with |Tref Tmut _ -> true | _ -> false)
	| _ -> false)
(***************************************)

and is_lvalue env = function
	| Evar id when Hashtbl.mem env.evar id && (let t,m,_ = Hashtbl.find env.evar id in m || (match below t with Tstructgeneric _ -> true |_ -> false)) -> true
	| Eindex (v, e) -> is_mutable env v || (let tv = (type_check_expr env v) in match tv with 
											|Tref Tstructgeneric _ |Tref Tmut Tstructgeneric _ -> true |_ -> false)
	| Estruct (e, id) -> is_mutable env e
	| Eunop (Unstar, eref) -> let teref = type_check_expr env eref in 
														(match teref with | Tref _ |Tmut Tref _ -> true | _ -> false )
	| _ -> false

and check_struct_fields env larg lid = if not (List.for_all (fun x -> List.exists (fun y -> x.name_struct_arg = fst y && x.type_struct_arg = (type_check_expr env (snd y))) lid) larg) then raise (Type_Error "Incoherent structure assignment")


let rec check_nodouble s = function
	|[] -> true
	|h :: t -> not(Sset.mem h s) && check_nodouble (Sset.add h s) t



let rec check_welltyped = function
	| Tnull | Tint | Tbool -> true
	| Tref t -> check_welltyped t
	| Tmut t -> check_welltyped t
	| Tstruct id -> Hashtbl.mem genv_struct id
	| Tstructgeneric (_, t) -> check_welltyped t
	| _ -> false


let type_check_fndecl df = let lnames = List.map (fun x -> x.name_arg) df.def_func in if not (check_nodouble Sset.empty lnames) then raise 
(Type_Error "Names of function parameters must be distinct");
														if List.exists (fun x -> not (check_welltyped x.type_arg)) df.def_func then raise (Type_Error "Ill defined type for function parameter");
														if not (check_welltyped df.return_func) then raise (Type_Error "Ill defined type for function return");
														let env = {evar = Hashtbl.create 17; level = 0; ref_lifespans = Hashtbl.create 5} in
														List.iter (fun x -> Hashtbl.add env.evar x.name_arg (x.type_arg, x.mut_arg, Plein)) df.def_func	; 
														Hashtbl.add genv_func df.name_func {args=df.def_func; return = df.return_func} ; 
														let tb = type_check_block env df.body_func in
															if not (usable_as tb df.return_func) then raise (Type_Error "Incorrect return type for function ") ;
															if not (check_noborrow df.return_func) then raise (Type_Error "Function return type should contain no borrows");
 print_string ("function " ^ df.name_func ^ "(" ); List.iter (fun a -> print_type a.type_arg) df.def_func;print_endline ")"


let type_check_decl decl =  match decl with
	|Decl_fun df -> type_check_fndecl df
	|Decl_struct ds -> if not (List.for_all (fun x -> check_welltyped x.type_struct_arg) ds.def_struct) then raise (Type_Error "Incorrectly typed field in structure declaration");
											if not (List.for_all (fun x -> check_noborrow x.type_struct_arg) ds.def_struct) then raise (Type_Error "Borrow types are not allowed in structure fields");
														Hashtbl.add genv_struct ds.name_struct ds.def_struct


let rec check_main = function
	|(Decl_fun df) :: t -> (df.name_func = "main" && List.length df.def_func = 0 && df.return_func = Tnull) || check_main t
	|h :: t -> check_main t	
	|_ -> false


let gen_type_check p = print_int (List.length p); print_endline " declarations found, proceeding to type checking";
if not (check_main p) then raise (Type_Error "Program must contain main function with profile fn main()");
List.iter type_check_decl p
(*take a program and check their function*)
	 




