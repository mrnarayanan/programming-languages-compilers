(*
 * File: mp3.ml
 *)

open Common

(* Problem 1*)
let asMonoTy1 () = failwith "Not implemented"
let asMonoTy2 () = failwith "Not implemented"
let asMonoTy3 () = failwith "Not implemented"
let asMonoTy4 () = failwith "Not implemented"

(* Problem 2*)
let rec subst_fun subst m = match subst with [] -> TyVar m | 
                                        (x::xs) -> let (var,ty) = x in if var = m then ty else subst_fun xs m;;

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy = match monoTy with
                                            TyVar t -> subst_fun subst t |
                                            TyConst (s,tl) -> TyConst (s, List.map (fun x -> monoTy_lift_subst subst x) tl);;


(* Problem 4*)
let rec occurs x ty = match ty with 
                        TyVar t -> if x = t then true else false | 
                        TyConst (s,tl) -> match tl with [] -> false | (l::ls) -> if (TyVar x) = l then true else occurs x (TyConst(s,ls));; 

(* Problem 5*)
let rec unify eqlst = failwith "Not implemented"

(* Extra Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
