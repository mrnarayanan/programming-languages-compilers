(* CS421 - Fall 2019
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let rec product l = match l with [] -> 1.0 | (x::xs) -> x *. product xs;;

(*Problem 2*)
let rec double_all l = match l with [] -> [] | (x::xs) -> (x +. x :: double_all xs);;

(*Problem 3*)
let rec pair_with_all x l = match l with [] -> [] | (t::ts) -> ((x,t) :: (pair_with_all x ts));;

(*Problem 4*)
let rec interleave l1 l2 = match l1 with [] -> 
                                (match l2 with [] -> [] | (y::ys) -> y::(interleave [] ys))
                            | (x::xs) ->
                                (match l2 with [] -> x::(interleave xs []) | (y::ys) -> x::y::(interleave xs ys));;

(*Problem 5*)
let rec sub_list l1 l2 = match l2 with [] -> true | 
                            (y::ys) -> 
                                (match l1 with [] -> false | (x::xs) -> (if y = x then (sub_list xs ys) else (sub_list xs l2)));;

(*Problem 6*)
let rec even_count_fr l = match l with [] -> 0 | (x::xs) -> let n = (even_count_fr xs) in n + if x mod 2 = 0 then 1 else 0;;

(*Problem 7*)
let rec pair_sums l = match l with [] -> [] | (x::xs) -> let f = (pair_sums xs) in 
                                                (let a,b = x in a+b)::f;;

(*Problem 8*)
let rec remove_even list = match list with [] -> [] | (x::xs) -> 
                                                        (let a = (remove_even xs) in if x mod 2 = 0 then a else x::a);;

(*Problem 9*)
let rec sift p l = match l with [] -> ([],[]) | (x::xs) -> let f = (sift p xs) in 
                                                        if p x then match f with (a,b) -> (x::a, b) else match f with (a,b) -> (a,x::b);;

(*Problem 10*)
let rec apply_even_odd l f g = match l with [] -> [] | (x::xs) -> 
                                                        let b = (apply_even_odd xs g f) in (f x)::b;;

(*Problem 11*)
let rec even_count_tr l = let rec aux list acc = match list with [] -> acc | (x::xs) -> if x mod 2 = 0 then aux xs (1+acc) else aux xs acc 
                            in aux l 0;;

(*Problem 12*)
let rec count_element l m = let rec aux list num acc = match list with [] -> acc | (x::xs) -> if x = num then aux xs num (1+acc) else aux xs num acc
                            in aux l m 0;;

(*Problem 13*)
let rec all_nonneg list = let rec aux list acc = match list with [] -> acc | (x::xs) -> if x < 0 then aux xs (1+acc) else aux xs acc 
                            in if (aux list 0) = 0 then true else false;;

(*Problem 14*)
let rec split_sum l f = let rec aux list func acc = match list with [] -> acc | (x::xs) -> if func x then 
                                                        match acc with (a,b) -> (aux xs func (a+x,b)) else 
                                                        match acc with (a,b) -> (aux xs func (a,b+x)) in
                                                    aux l f (0,0);;

(*Problem 15*)
let rec concat s list = let rec aux s ls acc = match ls with [] -> acc | 
                                                            (x::xs) -> if xs = [] then aux s xs (acc^x) else aux s xs (acc^x^s)
                            in aux s list "";;

(*Problem 16*)
let even_count_fr_base = 0;;
let even_count_fr_rec x rec_val = if x mod 2 = 0 then 1 + rec_val else rec_val;;
  
(*Problem 17*)
let pair_sums_map_arg p = let a,b = p in a+b;;

(*Problem 18*)
let remove_even_base = [];;
let remove_even_rec n r = if n mod 2 = 0 then r else n::r;;

(*Problem 19*)
let even_count_tr_start = 0;;
let even_count_tr_step acc_val x = if x mod 2 = 0 then 1 + acc_val else acc_val;;

(*Problem 20*)
let split_sum_start = (0,0);;
let split_sum_step f p n = let a,b = p in if f n then (a+n,b) else (a,b+n);;
