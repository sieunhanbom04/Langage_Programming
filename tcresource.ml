
open Ast
open Tcstatic
open Location
open Printf

module Sset = Set.Make(String)


type status = Vide | Plein | Borrow | Borrowmut

type env_var = (string, (typ * bool * status)) Hashtbl.t (* The boolean indicates whether the variable is mutable *)
type env_struct = (string, (struct_argument list)) Hashtbl.t
type func = {args : argument list; return : typ}
type env_func = (string, func) Hashtbl.t


let genv_struct = Hashtbl.create 17 (*global environment for structures*)
let genv_func : env_func = Hashtbl.create 17 (*global environment for functions*)


type env = {mutable evar : env_var; mutable level : int; mutable lifespans : (string, int) Hashtbl.t} (*local environment when typechecking a function )*)

exception RType_Error of string * location

let is_move = function 
	| Tref Tmut _ | Tstructgeneric _ | Tstruct _ -> true
	|_ -> false

let check_ownership env v = match v with 
	|TEvar (w, l, _) -> let _, _, stat = Hashtbl.find env.evar w in if stat = Vide then raise (RType_Error ("Access to void variable : " ^ w, l)) 
	| _ -> ()


let check_and_delete_ownership env e = match e with 
	|TEvar (v, l, _) -> let tv,mv,stat = Hashtbl.find env.evar v in 
													if stat = Vide then raise (RType_Error ("Access to void variable : " ^ v, l)); 
													if is_move tv then (Hashtbl.replace env.evar v (tv, mv, Vide); print_endline ("ownership deleted for variable : "^v))
	| TEindex (v, _, _, _) -> check_ownership env v
  | _ -> ()

let rec check_lifespan env te = match te with 
	| TEvar (v, l, _) -> (try let life = Hashtbl.find env.lifespans v in
											if env.level < life then raise (RType_Error ("Use of an expired variable/reference : " ^ v, l))
											with Not_found -> print_endline ("Variable lifespan not found "^ v))
	| TEindex (v, _, _, _) -> check_lifespan env v
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


let rec rtype_check_instr env = function (*type checking for instructions*)

  | TInothing _ -> ()
  | TIexpr (e, _, _) -> rtype_check_expr env e
  | TIexAssign (id, te, m, loc, t) -> let ty = extract_type_expr te in Hashtbl.add env.evar id ((if m then (match ty with Tmut _ -> ty |_ -> Tmut ty) else ty), m, Plein)(*; print_endline ("tc : adding variable " ^ id) *); 
  											Hashtbl.add env.lifespans id env.level;
												rtype_check_expr env te;
													
														check_and_delete_ownership env te; 
												begin match te with 
												|TEunop (Unmutp, TEvar (var, l, tt), _, _) -> if m then check_ownership env (TEvar (var, l, tt)); 
												let a,b,stat = Hashtbl.find env.evar var in 
												if stat = Borrowmut then raise (RType_Error ("Only one mutable Borrow can be made of a variable", l)) ; 
												
												
if m then (if not (ris_mutable env (TEvar (var, l, tt))) then raise (RType_Error ("Cannot mutably borrow a non mutable variable", l))) else (if stat = Vide then raise (RType_Error ("Cannot borrow a void variable", l)));
												
												
												Hashtbl.replace env.evar var (a, b, Borrowmut);
												
												|TEunop (Unp, TEvar (var, l, tt), _, _) -> if m then check_ownership env (TEvar (var, l, tt));
												let a,b,stat = Hashtbl.find env.evar var in 
												Hashtbl.replace env.evar var (a, b, (if stat = Borrowmut then stat else Borrow)); print_endline ("variable "^var^" is Borrowed");
												|_ -> ()
												end	

  | TIstAssign (idv, idst, lid, m, _, _) -> List.iter (fun x -> check_and_delete_ownership env (snd x)) lid;
										 												Hashtbl.add env.evar idv (Tstruct idst, m, Plein);
										 												Hashtbl.add env.lifespans idv env.level
												

  | TIwhile (texpr, tbl, _, _) -> rtype_check_expr env texpr; rtype_check_block env tbl
  | TICreturn (_, texpr, _, _) ->  rtype_check_expr env texpr
  | TICreturnNull _ -> () 
  | TIcond (tcond, _, _) -> rtype_check_if env tcond
	| _ -> () (***This will correspond to Ireturn and IreturnNull but they shouldn't occur***)

and rtype_check_expr env = function

  | TEconst _ | TEbool _ -> ()
  | TEvar _ as v -> check_lifespan env v
  | TEbinop (b, te1, te2, _, _) -> rtype_check_binop env b te1 te2
  | TEunop (u, te, _, _) -> rtype_check_unop env u te
  | TEstruct (te, id, _, _) -> check_lifespan env te; check_ownership env te (*********************************************)


  | TElength (te, _, _) -> check_lifespan env te
  | TEindex (tv, te, _, _) -> check_lifespan env tv ;check_ownership env tv ; rtype_check_expr env te
  
	| TEcall (id, l, _, _) -> List.iter (fun x -> check_and_delete_ownership env x; check_lifespan env x) l

  | TEprint _ -> ()
  | TEvector (l, _, _) -> if List.length l = 0 then () else
								 let lt = List.map (extract_type_expr) l in
									List.iter2 (fun x y -> check_lifespan env x; ( match x with | TEvar (v, loc, _) as var -> if is_move y then let tv, mv, statv = Hashtbl.find env.evar v in if statv <> Plein then raise (RType_Error ("Cannot move a non owning variable in vector declaration", loc)) else Hashtbl.replace env.evar v (tv, mv, Vide) | _ -> ()) ) l lt
																(************************************* *)
  | TEblock (bl, _, _) -> rtype_check_block env bl

and rtype_check_block env = function
  | TCFullBlock (l, exp, _, _) -> env.level <- env.level +1; 
														List.iter (fun x -> rtype_check_instr env x; () ) l;
														rtype_check_expr env exp;
														Hashtbl.iter (fun x y -> if y >= env.level then Hashtbl.replace env.lifespans x 10000) env.lifespans;
														env.level <- env.level -1
  | TCBlock (l, _, _) -> env.level <- env.level +1; 
								List.iter (rtype_check_instr env) l; 
								Hashtbl.iter (fun x y -> if y >= env.level then Hashtbl.replace env.lifespans x 10000) env.lifespans;
								env.level <- env.level -1


and rtype_check_if env = function 
  | TCif (e, b1, None, _, _) -> rtype_check_expr env e;
				  														rtype_check_block env b1;
  | TCif (e, b1, Some (b2), _, _) -> rtype_check_expr env e;
				  														rtype_check_block env b1;
																			rtype_check_block env b2;

  | TCnestedIf (e, bl, c, _, _) -> rtype_check_expr env e;
				  													rtype_check_block env bl;
																		rtype_check_if env c

and rtype_check_unop env u e = rtype_check_expr env e; check_lifespan env e; match u with
  | Uneg | Unot | Unp | Unmutp -> ()
  | Unstar -> (match e with TEvar (v, l, _) -> print_endline "checking reference lifespan";try if env.level < (Hashtbl.find env.lifespans v) then raise (RType_Error ("Use of an expired reference", l)) with Not_found -> () |_ -> ()) 
(*  | _ -> raise (RType_Error "Incorrect unary operator ") *)


and rtype_check_binop env b e1 e2 = check_lifespan env e1; check_lifespan env e2; match b with
  | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge | Band | Bor -> ()

	|Bassign -> (match e2 with 
										|TEunop(Unmutp, (TEvar (v, loc, _)), _, _) -> let tv, mv, statv = Hashtbl.find env.evar v in 
																								if statv = Borrowmut then raise (RType_Error ("Only one mutable Borrow can be made of a variable", loc));
																								if statv = Vide then raise (RType_Error ("Cannot borrow a void variable", loc));
																								Hashtbl.replace env.evar v (tv, mv, Borrowmut);
																								(match e1 with TEvar(w, _, _) -> Hashtbl.replace env.lifespans w (Hashtbl.find env.lifespans v) |_ -> ())

										|TEunop(Unp, (TEvar (v, loc, _)), _, _) -> let tv, mv, statv = Hashtbl.find env.evar v in 
																							if statv = Vide then raise (RType_Error ("Cannot borrow a void variable", loc));
																	
																							Hashtbl.replace env.evar v (tv, mv, (if statv = Borrowmut then statv else Borrow));
																							
																							(match e1 with TEvar (w, _, _) -> Hashtbl.replace env.lifespans w (Hashtbl.find env.lifespans v) |_ -> ())
	
										|TEvar (v, loc, _)  ->	let tv, mv, statv = Hashtbl.find env.evar v in 
																if statv <> Plein then raise (RType_Error ("Cannot assign the content of a non owning variable", loc));
																if (is_move tv) then Hashtbl.replace env.evar v (tv, mv, Vide)
										| _ -> ()
													(**Should we manage assignments to vector cells ???**)										

							);
							(match e1 with 
									|TEvar (w, loc, _) -> let tw, mw, statw = Hashtbl.find env.evar w in if (statw = Borrow || statw = Borrowmut) then raise (RType_Error ("Cannot modify a borrowed variable", loc)) else print_endline ("variable "^w^" is not Borrowed");
									Hashtbl.replace env.evar w (tw, mw, Plein)
							| _ -> ()
)

and ris_mutable env exp = match (extract_type_expr exp) with |Tmut _ | Tref Tmut Tstructgeneric _ -> true | _ -> (match exp with
	| TEvar (idd, _, _) -> let t,m,_ = Hashtbl.find env.evar idd in m
	| TEstruct (st, id, _, _) -> ris_mutable env st
	| TEindex (v, e, _, _) -> ris_mutable env v
  | TEunop (Unstar, e, _, _) -> (match (extract_type_expr e) with |Tref Tmut _ -> true | _ -> false)
	| _ -> false)
(***************************************)



let rtype_check_fndecl df = let env = {evar = Hashtbl.create 17; level = 0; lifespans = Hashtbl.create 17} in
														List.iter (fun x -> Hashtbl.add env.evar x.name_arg (x.type_arg, x.mut_arg, Plein); Hashtbl.add env.lifespans x.name_arg 1;
												) df.def_tfunc	; 
														Hashtbl.add genv_func df.name_tfunc {args=df.def_tfunc; return = df.return_tfunc} ; 
														rtype_check_block env df.body_tfunc;
														print_string ("function " ^ df.name_tfunc ^ "(" ); List.iter (fun a -> print_type a.type_arg) df.def_tfunc;print_endline ")"


let rtype_check_decl = function
	|TDecl_fun df -> rtype_check_fndecl df
	|TDecl_struct ds -> Hashtbl.add genv_struct ds.name_struct ds.def_struct



let resource_type_check = List.iter rtype_check_decl
(*take a program and check their function*)
	 




