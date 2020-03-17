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
       with TyVar n -> (* Eliminate *)
         unify_eliminate n t rem_constraints unify
         | TyConst(c,args) ->  (* Orient and Decompose *)
           raise (Failure "Not implemented yet.") (* Remove this *)


  (* Your code here *)



  )



