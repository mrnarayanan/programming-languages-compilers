open Common
open Plsolution
(* PUT ANY HELPER FUNCTIONS YOU WANT UP HERE! *)

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
           raise (Failure "Not implemented yet.") (* Remove this *)


  (* Your code here *)



  )



