
open Ast
open Printf
open Location
module Sset = Set.Make(String)


type tinstruction =
  | TInothing of location * typ
  | TIexpr of texpr * location * typ
  | TIexAssign of ident * texpr * bool * location * typ
  | TIstAssign of ident * ident * ((ident * texpr) list ) * bool * location * typ
  | TIwhile of texpr * tblock * location * typ
  | TIreturn of texpr * location * typ
  | TICreturn of ident * texpr * location * typ
  | TIreturnNull of location * typ
  | TICreturnNull of ident * location * typ
  | TIcond of tcondition * location * typ
and texpr =
  | TEconst of int * location * typ
  | TEbool of bool * location * typ
  | TEvar of ident * location * typ
  | TEbinop of binop * texpr * texpr * location * typ
  | TEunop of unop * texpr * location * typ
  | TEstruct of texpr * ident * location * typ
  | TElength of texpr * location * typ
  | TEindex of texpr * texpr * location * typ
  | TEprint of string * location * typ
  | TEcall of ident * texpr list * location * typ
  | TEvector of texpr list * location * typ
  | TEblock of tblock * location * typ
and tblock =
  | TCFullBlock of tinstruction list * texpr * location * typ
  | TCBlock of tinstruction list * location * typ
and tcondition =
  | TCif of texpr * tblock * (tblock option) * location * typ
  | TCnestedIf of texpr * tblock * tcondition * location * typ
  
type tdecl_fun = {
  name_tfunc : string;
  def_tfunc : argument list;
  return_tfunc : typ;
  body_tfunc : tblock;
  loc_tfunc: location;
}
type tdecl = TDecl_fun of tdecl_fun
            | TDecl_struct of decl_struct

type tprogram = tdecl list

  
let extract_type_instr = function
  | TInothing (_, t) -> t
  | TIexpr (_,_,t) -> t
  | TIexAssign (_, _, _, _, t )-> t
  | TIstAssign (_, _,_, _, _, t) -> t
  | TIwhile (_, _, _, t) -> t
  | TIreturn (_, _, t )-> t
  | TICreturn (_, _, _, t) -> t
  | TIreturnNull (_, t) -> t
  | TICreturnNull (_, _, t) -> t
  | TIcond (_, _, t) -> t
let extract_type_expr = function
  | TEconst (_, _, t) -> t
  | TEbool (_, _, t) -> t
  | TEvar (_, _, t) -> t
  | TEbinop (_, _, _, _, t) -> t
  | TEunop (_, _, _, t) -> t
  | TEstruct (_, _, _, t) -> t
  | TElength (_, _, t) -> t
  | TEindex (_, _, _, t) -> t
  | TEprint (_, _, t) -> t
  | TEcall (_, _, _, t) -> t
  | TEvector (_, _, t) -> t
  | TEblock (_, _, t) -> t
let extract_loc_expr = function
  | TEconst (_, l, _) -> l
  | TEbool (_, l, _) -> l
  | TEvar (_, l, _) -> l
  | TEbinop (_, _, _, l, _) -> l
  | TEunop (_, _, l, _) -> l
  | TEstruct (_, _, l, _) -> l
  | TElength (_, l, _) -> l
  | TEindex (_, _, l, _) ->l
  | TEprint (_, l, _) -> l
  | TEcall (_, _, l, _) -> l
  | TEvector (_, l, _) -> l
  | TEblock (_, l, _) -> l
let extract_type_block = function
  | TCFullBlock (_, _, _, t) -> t
  | TCBlock (_, _, t) -> t
and extract_type_condition = function
  | TCif (_, _, _, _, t) -> t
  | TCnestedIf (_, _, _, _, t) -> t




type env_var = (string, (typ * bool)) Hashtbl.t (* The boolean indicates whether the variable is mutable *)
type env_struct = (string, (struct_argument list)) Hashtbl.t
type func = {args : argument list; return : typ}
type env_func = (string, func) Hashtbl.t


let genv_struct = Hashtbl.create 17 (*global environment for structures*)
let genv_func : env_func = Hashtbl.create 17 (*global environment for functions*)


type env = {mutable evar : env_var} (*local environment when typechecking a function )*)

exception Type_Error of string * location

let rec below ty = match ty with |Tref t -> below t | Tmut t -> below t | _ -> ty

let is_move = function 
	| Tref Tmut _ | Tstructgeneric _ | Tstruct _ -> true
	|_ -> false

(*
let check_and_delete_ownership env e = match e with 
	|Evar v -> let tv,mv,stat = Hashtbl.find env.evar v in 
													if stat = Vide then raise (Type_Error ("Access to void variable : " ^ v)); 
													if is_move tv then Hashtbl.replace env.evar v (tv, mv, Vide) ; print_endline ("variable "^v^" loses ownership")
	| _ -> ()
*)
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
(*
let check_ownership env v = match v with 
	|Evar w -> let _, _, stat = Hashtbl.find env.evar w in if stat = Vide then raise (Type_Error ("Access to void variable : " ^ w)) 
	| _ -> ()
*)

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

  | Inothing l -> TInothing (l, Tnull)
  | Iexpr (e, l) -> print_endline "typechecking Iexpr"; let te = type_check_expr env e in TIexpr (te, l, Tnull)
  | IexAssign (id, e, m, loc) -> let texp = type_check_expr env e in 
  																let typ_texp = extract_type_expr texp in
  																Hashtbl.add env.evar id ((if m then (match typ_texp with Tmut _ -> typ_texp |_ -> Tmut typ_texp) else typ_texp), m)(*; print_endline ("tc : adding variable " ^ id) *); 
  																TIexAssign (id, texp, m, loc, Tnull)

  | IstAssign (idv, idst, lid, m, loc) -> if not (Hashtbl.mem genv_struct idst) then raise (Type_Error ("Undeclared structure " ^ idst, loc));
										let larg = Hashtbl.find genv_struct idst in
										check_struct_fields env larg lid loc;
										let tlid = List.map (fun (x, y) -> (x, type_check_expr env y)) lid in
										 Hashtbl.add env.evar idv (Tstruct idst, m);
										 TIstAssign (idv, idst, tlid, m, loc, Tnull)

  | Iwhile (expr, bl, loc) -> let texpr = type_check_expr env expr in
  														if (extract_type_expr texpr) <> Tbool then raise (Type_Error ("condition should be boolean", extract_loc_expr texpr));
  														TIwhile (texpr, type_check_block env bl, loc, Tnull)
  														
  | ICreturn (f,e, loc) -> let te = type_check_expr env e in
											let tte = extract_type_expr te in
											let df = Hashtbl.find genv_func f in 
											if not (usable_as tte df.return) then raise (Type_Error ("Incorrect return type for function", loc));
											TICreturn (f, te, loc, tte)

  | ICreturnNull (f, loc) -> let df = Hashtbl.find genv_func f in if not (usable_as Tnull df.return) then raise (Type_Error ("Incorrect return type for function", loc)); TICreturnNull(f, loc, Tnull)
   
  | Icond (cond, loc) -> let tcond = type_check_if env cond in TIcond (tcond, loc, extract_type_condition tcond)
	(*| _ -> raise (Type_Error ("Unexpected instruction (probably Ireturn or Ireturn Null)", let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
{ loc_start = loc; loc_end = loc; loc_ghost = false })) (***This will correspond to Ireturn and IreturnNull but they shouldn't occur***)
*)
and type_check_expr env e= print_endline "typechecking an expression";match e with

  | Econst (i, l) -> TEconst(i, l, Tint)
  | Ebool (b , l) -> TEbool(b , l ,Tbool)
  | Evar (id, l) -> if (Hashtbl.mem env.evar id) then let xx, _ = (Hashtbl.find env.evar id) in
  								TEvar (id, l, xx) 
  								else raise (Type_Error ("Unbound variable " ^ id, l))
  | Ebinop (b, e1, e2, loc) -> print_endline "static checking binop expr"; let t, te1, te2 = type_check_binop env b e1 e2 loc in
  														TEbinop (b , te1 , te2, loc, t)
  | Eunop (u, e, loc) -> let t, te = type_check_unop env u e loc in
  											 TEunop(u, te, loc, t)
  | Estruct (e, id, loc) -> let texp = type_check_expr env e in (match below (extract_type_expr texp) with 
												|Tstruct st -> let stl = Hashtbl.find genv_struct st in
												try let elem = (List.find (fun xx -> xx.name_struct_arg = id) stl) in TEstruct(texp, id, loc, elem.type_struct_arg) with Not_found -> raise (Type_Error ("Unknown structure field", loc))
												| _ -> raise (Type_Error ("Field indexing is only possible for structures", loc)))  

  | Elength (e, l) -> let texp = type_check_expr env e in (match below (extract_type_expr texp) with 
									|Tstructgeneric _ -> TElength (texp, l, Tint)
									| _ -> raise (Type_Error ("length method can only be used on vectors", l)) )
  | Eindex (v, e, l) -> let te = type_check_expr env e in
  											if (extract_type_expr te) <> Tint then raise (Type_Error ("Index should be integer", extract_loc_expr te));
  											let tv = type_check_expr env v in
												(match (extract_type_expr tv) with 
											|Tstructgeneric (_, t) |Tmut Tstructgeneric (_, t) |Tref Tstructgeneric (_, t) |Tref Tmut Tstructgeneric (_, t) -> TEindex(tv, te, l, t)
											| _ -> (*print_type tv; *)raise (Type_Error ("Unbound vector identifier or indexing of non vector variable", l)))
		     
	| Ecall (id, l, loc) -> let f = try Hashtbl.find genv_func id with Not_found -> raise (Type_Error ("Undeclared function " ^ id, loc)) in 
											let ltexp = List.map (type_check_expr env) l in
											let lty  = List.map (fun ar -> ar.type_arg) f.args in
											(try List.iter2 (fun a b -> let ta = extract_type_expr a in if not (usable_as ta b) then raise (Type_Error ("Incorrect argument type in function " ^ id ^ " " ^ (str_type ta) ^ " not usable as "^(str_type b), extract_loc_expr a))) ltexp lty with (Invalid_argument "List.iter2") -> raise (Type_Error ("Too many/few arguments for function "^id, loc)))
											; TEcall (id, ltexp, loc, f.return)

  | Eprint (s, l) -> TEprint (s, l, Tnull)
  | Evector (l, loc) -> if List.length l = 0 then TEvector ([], loc, Tstructgeneric ("vec",Tnull)) else
											let ltexp = List.map (type_check_expr env) l in
											let lt = List.map (extract_type_expr) ltexp in
											let tgen = try List.find (fun x -> List.for_all (fun y -> usable_as x y) lt) lt with Not_found -> raise (Type_Error ("Non homogenous vector", loc)) in TEvector (ltexp, loc, Tstructgeneric ("vec", tgen)) 
																(************************************* *)
  | Eblock (bl, loc) -> let tbl = type_check_block env bl in 
  										TEblock (tbl, loc, extract_type_block tbl)

and type_check_block env = function
  | CFullBlock (l, exp, loc) -> print_endline "type checking CFullBlock"; let lt = List.map (fun x -> type_check_instr env x ) l in
																let texp = type_check_expr env exp in
																print_string "CfullBlock typed : ";print_type (extract_type_expr texp);
																TCFullBlock (lt, texp, loc, extract_type_expr texp)
  | CBlock (l, loc) -> let lt = List.map (type_check_instr env) l in 
								if List.length l = 0 then TCBlock (lt, loc, Tnull) else
								let rlt = (List.rev lt) in
  							TCBlock (lt, loc, extract_type_instr (List.hd rlt))


and type_check_if env = function 
  | Cif (e, b1, None, loc) -> let texp = type_check_expr env e in
  													(if (extract_type_expr texp) <> Tbool then raise (Type_Error ("condition should be boolean", extract_loc_expr texp)));
  													let tb1 = type_check_block env b1 in
  													let ttb1 = extract_type_block tb1 in
  													TCif (texp, tb1, None, loc, ttb1)
  | Cif (e, b1, Some (b2), loc) -> let texp = type_check_expr env e in
  													(if (extract_type_expr texp) <> Tbool then raise (Type_Error ("condition should be boolean", extract_loc_expr texp)));
  													let tb1 = type_check_block env b1 in
  													let tb2 = type_check_block env b2 in
  													let ttb1 = extract_type_block tb1 in
  													if ttb1 <> (extract_type_block tb2) then raise (Type_Error ("different types in if alternatives", loc));
  													TCif (texp, tb1, Some (tb2), loc, ttb1)
  | CnestedIf (e, bl, c, loc) -> let texp = type_check_expr env e in
			  													(if (extract_type_expr texp) <> Tbool then raise (Type_Error ("condition should be boolean", extract_loc_expr texp)));
		  													 let tbl = type_check_block env bl in
		  													 let tc = type_check_if env c in
		  													 let ttbl = extract_type_block tbl in
		  													 if ttbl <> (extract_type_condition tc) then raise (Type_Error ("different types in if alternatives", loc)) else
		  													 TCnestedIf (texp, tbl, tc, loc, ttbl)


and type_check_unop env u e loc = let texp = type_check_expr env e in
															let te = extract_type_expr texp in match u with
  | Uneg when te = Tint -> (Tint, texp) (* -e *)
  | Unot when te = Tbool -> (Tbool, texp)(* not e *)
  | Unstar -> let tr = (match te with | Tref ty -> ty | Tmut Tref ty -> ty | _ -> raise (Type_Error ("Dereferencing operator applied to non reference value", loc)) ) in (tr, texp)
  | Unp -> (match e with Eunop(Unstar, _, _) |Evar _ -> () | _ -> raise (Type_Error ("Reference cannot be created for non variables", loc))); (Tref te, texp) 
  | Unmutp -> if not (is_lvalue env e) then raise (Type_Error ("References can only be created for leftvalues", loc));
							if not (is_mutable env e) then raise (Type_Error ("Mutable references can only be created for mutable values", loc));
							(Tref (match te with Tmut _ -> te |_-> Tmut te), texp) (************************************* *)
  | _ -> raise (Type_Error ("Incorrect unary operator ", loc)) 

and type_check_binop env b e1 e2 loc =
	let te1 = (type_check_expr env e1) in
	let te2 = (type_check_expr env e2) in 
  let tte1 = extract_type_expr te1 in
  let tte2 = extract_type_expr te2 in
	let nonint = not (usable_as tte1 Tint && usable_as tte2 Tint) in
	match b with
  | Badd | Bsub | Bmul | Bdiv | Bmod -> if nonint then raise (Type_Error ("Non integer in arithmetic expression", loc)); (Tint, te1, te2)
  | Beq | Bneq | Blt | Ble | Bgt | Bge -> if nonint then raise (Type_Error ("Comparison with non integer", loc)); (Tbool, te1, te2) (* == != < <= > >= *)
  | Band | Bor -> if tte1 <> Tbool || tte2 <> Tbool then 
		  raise (Type_Error ("Logical operator should be applied to bool values", loc)); (Tbool, te1, te2)
	|Bassign -> let me1 = is_mutable env e1 in
							if not (is_lvalue env e1) then raise (Type_Error ("Assigning to non left value", extract_loc_expr te1));
							if not (me1) then raise (Type_Error ("Assigning to non mutable variable", extract_loc_expr te1));
							if not (assignable tte2 tte1) then raise (Type_Error ("Assigning to incompatible type "^(str_type tte2)^" -> "^ (str_type tte1), loc)); (Tnull, te1, te2) 

and is_mutable env exp = let texp = extract_type_expr (type_check_expr env exp) in 
													match texp with |Tmut _|Tref Tmut _ -> true | _ -> (match exp with

	| Evar (idd, _) -> let _,m = Hashtbl.find env.evar idd in m
	| Estruct (st, id, _) -> is_mutable env st
	| Eindex (v, e, _) -> is_mutable env v
  | Eunop (Unstar, e, _) -> (match extract_type_expr (type_check_expr env e) with |Tref Tmut _ -> true | _ -> false)
	| _ -> false)
(***************************************)

and is_lvalue env = function
	| Evar (id, _) when Hashtbl.mem env.evar id && (let t,m = Hashtbl.find env.evar id in m || (match below t with Tstructgeneric _ -> true |_ -> false)) -> true
	| Eindex (v, e, _) -> is_mutable env v || (let tv = extract_type_expr (type_check_expr env v) in match tv with 
											(*|Tref Tstructgeneric _*) |Tref Tmut Tstructgeneric _ -> true |_ -> false)
	| Estruct (e, id, _) -> is_mutable env e || (match extract_type_expr (type_check_expr env e) with |Tmut _(*| Tref Tstruct _*) | Tref Tmut Tstruct _  -> true | _ -> false)
	| Eunop (Unstar, eref, _) -> let teref = extract_type_expr (type_check_expr env eref) in 
														(match teref with | Tref _ |Tmut Tref _ -> true | _ -> false )
	| _ -> false

and check_struct_fields env larg lid loc = if not (List.for_all (fun x -> List.exists (fun y -> x.name_struct_arg = fst y && x.type_struct_arg = extract_type_expr (type_check_expr env (snd y))) lid) larg) then raise (Type_Error ("Incoherent structure assignment", loc))


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
(Type_Error ("Names of function parameters must be distinct", df.loc_func));
														List.iter (fun x -> if not (check_welltyped x.type_arg) then raise (Type_Error ("Function : "^df.name_func^" , Ill defined type for parameter "^x.name_arg, x.loc_arg))) df.def_func;
														if not (check_welltyped df.return_func) then raise (Type_Error ("Function : "^df.name_func^" ,Ill defined return type", df.loc_func));
														let env = {evar = Hashtbl.create 17} in
														List.iter (fun x -> Hashtbl.add env.evar x.name_arg (x.type_arg, x.mut_arg)) df.def_func	; 
														Hashtbl.add genv_func df.name_func {args=df.def_func; return = df.return_func} ; 
														let tb = type_check_block env df.body_func in
															if not (usable_as (extract_type_block tb) df.return_func) then raise (Type_Error ("Incorrect return type for function "^df.name_func , df.loc_func)) ;
															if not (check_noborrow df.return_func) then raise (Type_Error ("Function : "^df.name_func^" , return type should contain no borrows", df.loc_func));
															 print_string ("function " ^ df.name_func ^ "(" ); List.iter (fun a -> print_type a.type_arg) df.def_func;print_endline ")";
															 TDecl_fun {name_tfunc = df.name_func ; def_tfunc = df.def_func ; return_tfunc = df.return_func; body_tfunc = tb ; loc_tfunc = df.loc_func}
															 
 


let type_check_decl = function
	|Decl_fun df -> type_check_fndecl df
	|Decl_struct ds -> List.iter (fun x -> 
								if not (check_welltyped x.type_struct_arg) 
								then raise (Type_Error ("Incorrectly typed field in structure declaration", x.loc_struct_arg)) ; 
								
								if not (check_noborrow x.type_struct_arg) 
								then raise (Type_Error ("Borrow types are not allowed in structure fields", x.loc_struct_arg))) ds.def_struct;
											Hashtbl.add genv_struct ds.name_struct ds.def_struct;
											TDecl_struct ds


let rec check_main = function
	|(Decl_fun df) :: t -> (df.name_func = "main" && List.length df.def_func = 0 && df.return_func = Tnull) || check_main t
	|h :: t -> check_main t	
	|_ -> false


let gen_type_check p = print_int (List.length p); print_endline " declarations found, proceeding to type checking";
(*if not (check_main p) then raise (Type_Error ("Program must contain main function with profile fn main()", 
let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
{ loc_start = loc; loc_end = loc; loc_ghost = false }));*)
List.map type_check_decl p
(*take a program and check their function*)
	 




