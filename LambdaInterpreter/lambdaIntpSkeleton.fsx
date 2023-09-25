(* Michael R. Hansen, October 24, 2011                       *)
(*                    Revised October 21, 2013               *)
(* Skeleton program for a simple lambda-calculus interpreter *)

type Lambda = 
   | L of string * Lambda
   | A of Lambda * Lambda
   | V of string
   | O of string
   | I of int 
   | B of bool;; 


(* free: Lambda -> Set<string> *)
let rec free t = 
  match t with
  | V x      -> Set.singleton  x  
  | A(t1,t2) -> ...
  | L(x,t)   -> ...
  | _        -> ... ;;

(* nextVar: string -> string *)
let nextVar = 
   let n = ref 0
   let f x = let s = x + "#" + string(!n) 
             (n := !n+1; s)
   f;;

(* subst: Lambda -> string -> Lambda -> Lambda *)
let rec subst t x ta  = 
   match t with
   | L(y,t') when x=y                       -> ... 
   | L(y,t') when Set.contains y (free ...) -> ...
   | L(y,t')                                -> ...
   | V y       -> if x=y ...
   | A(t1,t2)  -> ...
   | _         -> t;;
 

(* One step normal-order reduction *)
(* red: Lambda -> Lambda option *)
let rec red t = 
    match t with 
    | A(A(O "=", I a), I b)           -> Some(B(a=b))
    | A(A(O "+", I a), I b)           -> ...
    | A(A(O "-", I a), I b)           -> ...
    | A(A(O "*", I a), I b)           -> ...
    | A(A(A(O "ite", B true),t1),t2)  -> ...
    | A(A(A(O "ite", B false),t1),t2) -> ... 
    | L(x,t)                          -> match red t with
                                         | None    -> ... 
                                         | Some t' -> ...
    | A(L(x,t1),t2)                   -> ...
    | A(t1,t2)                        -> match red t1 with 
                                         | Some t1' -> ...
                                         | None     -> ...
    | _                               -> None;; 

(* repeated normal-order reductions *)
(* reduce: Lambda -> Lambda *)                  
let rec reduce t = match red t with ... ;;

  
let t1 = L("x",L("y", A(A(O "+", V "x"), V "y")));;
let t2 = A(t1, I 3);;

let t3 = A(t2, I 4);;

let v3 = reduce t3;;

let Y = let t = ... 
        A(t,t);;

let F = L("f",L("n",A( ... ;;

let fact = A(Y,F);;
let fac8 = A(fact,I 8);;

let vfac8 = reduce fac8;;

