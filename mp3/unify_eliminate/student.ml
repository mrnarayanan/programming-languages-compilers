open Common
open Plsolution
(* PUT ANY HELPER FUNCTIONS YOU WANT UP HERE! *)
let new_eq n t rest = List.map (fun (x1,x2) -> (monoTy_lift_subst [(n,t)] x1, monoTy_lift_subst[(n,t)] x2)) rest;;

let rec unify constraints =
  match constraints with
    [] -> Some([])
  | (s,t)::rem_constraints ->
    (* Delete *)
    if s = t then unify rem_constraints
    else
      (match s
       with TyConst(c,args) ->
         unify_orient_or_decompose c args t rem_constraints unify
         | TyVar n -> (* Eliminate *)
           if (occurs n t) then None else match unify (new_eq n t rem_constraints) with 
                                                                                    None -> None |
                                                                                    Some(p) -> Some((n, monoTy_lift_subst p t)::p)
      )



