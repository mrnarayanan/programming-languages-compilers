(* File: ml3.ml *)

open Common

(* Problem 1 *)
let rec import_list lst = match lst with 
                            [] -> ConstExp NilConst |
                            (x::xs) -> let a,b = x in 
                                        BinOpAppExp(ConsOp, BinOpAppExp(CommaOp, ConstExp(IntConst a), ConstExp(IntConst b)), 
                                        import_list xs);;

(* Problem 2 *)
let pair_sums =  LetRecInExp("pair_sums","lst",
                    IfExp(BinOpAppExp(EqOp,VarExp "lst", ConstExp NilConst), 
                            ConstExp NilConst, 
                            LetInExp("x", 
                                    MonOpAppExp(HdOp,VarExp "lst"),
                                    BinOpAppExp(ConsOp,
                                        BinOpAppExp(IntPlusOp, 
                                            MonOpAppExp(FstOp,VarExp "x"),
                                            MonOpAppExp(SndOp,VarExp "x")),
                                        AppExp(VarExp "pair_sums", MonOpAppExp(TlOp,VarExp "lst"))))),
                    AppExp(VarExp "pair_sums", BinOpAppExp (ConsOp,
                                                    BinOpAppExp (CommaOp, ConstExp (IntConst 7), ConstExp (IntConst 1)),
                                                    BinOpAppExp (ConsOp,
                                                    BinOpAppExp (CommaOp, ConstExp (IntConst 4), ConstExp (IntConst 2)),
                                                    BinOpAppExp (ConsOp,
                                                    BinOpAppExp (CommaOp, ConstExp (IntConst 6), ConstExp (IntConst 3)),
                                                    ConstExp NilConst)))));;

(* Problem 3 *)
let rec count_const_in_exp exp =  match exp with 
                                    VarExp x -> 0 |
                                    ConstExp c -> 1 |
                                    MonOpAppExp (mon_op, exp1) -> count_const_in_exp exp1 |
                                    BinOpAppExp (bin_op, exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2) |
                                    IfExp (exp1, exp2, exp3) -> (count_const_in_exp exp1) + (count_const_in_exp exp2) + (count_const_in_exp exp3) |
                                    AppExp (exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2) | 
                                    FunExp (s, exp1) -> count_const_in_exp exp1 | 
                                    LetInExp (s, exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2) |
                                    LetRecInExp (s1, s2, exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2);;

(* Problem 4 *)
let rec freeVarsInExp exp = match exp with 
                                VarExp x -> [x] | 
                                ConstExp c -> [] | 
                                MonOpAppExp (mon_op, exp1) -> freeVarsInExp exp1 |
                                BinOpAppExp (bin_op, exp1, exp2) -> (freeVarsInExp exp1) @ (freeVarsInExp exp2) | 
                                IfExp (exp1, exp2, exp3) -> (freeVarsInExp exp1) @ (freeVarsInExp exp2) @ (freeVarsInExp exp3) |
                                AppExp (exp1, exp2) -> (freeVarsInExp exp1) @ (freeVarsInExp exp2) | 
                                FunExp (s, exp1) -> List.filter (fun y -> not (s = y)) (freeVarsInExp exp1) | 
                                LetInExp (s, exp1, exp2) -> (freeVarsInExp exp1) @ (List.filter (fun y -> not (s = y)) (freeVarsInExp exp2)) |
                                LetRecInExp (f, s, exp1, exp2) -> (List.filter(fun y -> not (s = y) && not (f = y)) (freeVarsInExp exp1)) @ (List.filter(fun y -> not (f = y)) (freeVarsInExp exp2));;

(* Problem 5 *)
let rec cps_exp e k = match e with
                        VarExp x -> VarCPS(k,x) |
                        ConstExp c -> ConstCPS(k,c) |
                        IfExp (exp1,exp2,exp3) -> let v = freshFor((freeVarsInContCPS k) @ (freeVarsInExp exp2) @ (freeVarsInExp exp3)) 
                                                in cps_exp exp1 (FnContCPS(v, IfCPS (v,cps_exp exp2 k,cps_exp exp3 k))) |
                        AppExp (exp1,exp2) -> let v2 = freshFor((freeVarsInContCPS k) @ (freeVarsInExp exp1)) 
                                                in let v1 = freshFor(v2::(freeVarsInContCPS k)) 
                                                    in cps_exp exp2 (FnContCPS(v2, cps_exp exp1 (FnContCPS(v1, AppCPS(k,v1,v2))))) |
                        BinOpAppExp (bin_op, exp1, exp2) -> let v2 = freshFor((freeVarsInContCPS k) @ (freeVarsInExp exp1)) 
                                                            in let v1 = freshFor(v2::(freeVarsInContCPS k)) 
                                                                in cps_exp exp2 (FnContCPS(v2, cps_exp exp1 (FnContCPS(v1, BinOpAppCPS(k,bin_op,v1,v2))))) | 
                        MonOpAppExp (mon_op, exp1) -> let v = freshFor(freeVarsInContCPS k) 
                                                    in cps_exp exp1 (FnContCPS(v, MonOpAppCPS(k,mon_op,v))) | 
                        FunExp (s,exp1) -> FunCPS(k,s,Kvar, cps_exp exp1 (ContVarCPS Kvar)) | 
                        LetInExp (s, exp1, exp2) -> cps_exp exp1 (FnContCPS(s, cps_exp exp2 k)) |
                        LetRecInExp (f, s, exp1, exp2) -> FixCPS(FnContCPS(f, cps_exp exp2 k),f,s,Kvar, cps_exp exp1 (ContVarCPS Kvar));;