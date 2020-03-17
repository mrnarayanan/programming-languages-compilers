open Common
open Plsolution
(* Leave these lines here! *)
(* Put any helper functions you want to write and use in this space. *)
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
