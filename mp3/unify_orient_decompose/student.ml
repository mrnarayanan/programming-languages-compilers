open Common
open Plsolution
(* PUT ANY HELPER FUNCTIONS YOU WANT UP HERE! *)
let rec add_new l1 l2 m = match l1,l2 with 
                            [],[] -> Some m |
                            h1::t1, h2::t2 -> add_new t1 t2 ((h1,h2)::m) |
                            _ -> None;;

let rec unify constraints =
  match constraints with
    [] -> Some([])
  | (s,t)::rem_constraints ->
    (* Delete *)
    if s = t then unify rem_constraints
    else
      (match s
       with TyVar n -> (* Eliminate *)
         unify_eliminate n t rem_constraints unify
         | TyConst(c,args) ->  (* Orient and Decompose *)
           match t with 
              TyConst(c2,args2) -> if c = c2 then (match (add_new args args2 rem_constraints) with None -> None | Some rem -> unify rem) else None |
              TyVar v -> unify ((TyVar v, TyConst (c,args))::rem_constraints)
      )



