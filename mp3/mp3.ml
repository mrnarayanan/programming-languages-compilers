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
let rec add_new list1 list2 m = match list1,list2 with 
                            [],[] -> Some m |
                            h1::t1, h2::t2 -> add_new t1 t2 ((h1,h2)::m) |
                            _ -> None;;

let new_eq n t rest = List.map (fun (x1,x2) -> (monoTy_lift_subst [(n,t)] x1, monoTy_lift_subst[(n,t)] x2)) rest;;
  

let rec unify constraints =
  match constraints with
    [] -> Some([])
  | (s,t)::rem_constraints ->
    (* Delete *)
    if s = t then unify rem_constraints
    else
      (match s
       with TyVar n -> (* Eliminate *)
          if (occurs n t) then None else match unify (new_eq n t rem_constraints) with 
                                                                                    None -> None |
                                                                                    Some(p) -> Some((n, monoTy_lift_subst p t)::p)
         | TyConst(c,args) ->  (* Orient and Decompose *)
           match t with 
              (*Orient*)
              TyVar v -> unify ((TyVar v, TyConst (c,args))::rem_constraints) |
              (*Decompose*)
              TyConst(c2,args2) -> if c = c2 then (match (add_new args args2 rem_constraints) with None -> None | Some rem -> unify rem) else None
    )


(* Extra Credit *)
let rec find p list = match list with [] -> None |
                                      (x::xs) -> if (p x) then Some x else find p xs;;

let rec recurs subst m ty = match ty with
                                  TyVar n -> (match find (fun x -> fst x = n) subst with 
                                                                                    Some(list1,list2) -> (subst, m, TyVar list2) |
                                                                                    None -> (((n,m)::subst), m+1, TyVar m)) |
                                  TyConst (c,tys) -> (match List.fold_left (fun (subst,n,ty1) -> fun ty -> 
                                                                          (match recurs subst n ty with (new_subst, new_n, new_ty) -> (new_subst, new_n, new_ty::ty1))) (subst, m, []) tys with
                                                                          (new_subst, new_m, new_tys) -> (new_subst, new_m, TyConst(c, List.rev new_tys)));;

let helper inp = let (_,_,tty) = recurs [] 0 inp in tty;;


let rec equiv_types ty1 ty2 = let ty1n = helper ty1 in 
                              let ty2n = helper ty2 in 
                              (ty1n = ty2n);;
