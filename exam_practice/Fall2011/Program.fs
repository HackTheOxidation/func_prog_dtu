
/// Problem 1
type name = string
type phone = int
type level = int

type description = phone * level
type register = (name * description)

// 1.1
let reg = [("Joe", (10101010, 4)); ("Sal", (11111111, 2)); ("Sam", (12121212, 7)); ("Jane", (13131313, 1))]

// 1.2
let rec getPhone (targetName: string) = function
  | [] -> failwith "Register"
  | (personName, (phoneNumber, _))::_ when targetName = personName -> phoneNumber
  | _::rest -> getPhone targetName rest

// 1.3
let rec delete (targetName, r) =
  match r with
    | [] -> []
    | (personName, _)::rest when targetName = personName -> rest
    | person::rest -> person::delete (targetName, rest)

// 1.4

// Auxillary predicate
let levelMatches (l: level) (_, (_, l')): bool = abs (l - l') < 3

// Point-free/Tacit solution
let getCandidates lvl =
  List.filter (levelMatches lvl) >> List.map (fun (n, (p, _)) -> (n, p))


/// Problem 2
type exp =
  | C of int
  | BinOp of exp * string * exp

// 2.1
let exp1 = C 2
let exp2 = BinOp (C 3, "+", C 5)
let exp3 = BinOp (C 2, "*", BinOp (C 7, "-", C 1))

// 2.2
let rec toString = function
  | C c -> $"{c}"
  | BinOp (l, op, r) -> "(" + toString l + op + toString r + ")"

// Tail-recursive, continuation-based approach
let toStringA tree =
  let rec toStringC acc = function
    | C c -> acc $"{c}"
    | BinOp (l, op, r) -> toStringC (fun left -> toStringC (fun right -> "(" + left + op + right + ")") r) l

  toStringC id tree
  
// 2.3
let rec extractOperators = function
  | C _ -> Set.empty
  | BinOp (l, op, r) -> Set.add op <| Set.union (extractOperators l) (extractOperators r)

// 2.4
module Problem2_4 =
  type exp =
    | C of int
    | BinOp of exp * string * exp
    | Id of string
    | Def of string * exp * exp

  let isDef expression =
    let rec isDefC vars c = function
      | C _ -> c true
      | Id x -> c <| Set.contains x vars
      | BinOp (l, _, r) ->
        isDefC vars (fun left -> isDefC vars (fun right -> left && right) r) l 
      | Def (x, l, r) ->
        let newVars = Set.add x vars
        isDefC vars (fun left -> isDefC newVars (fun right -> left && right) r) l

    isDefC Set.empty expression

/// Problem 3
type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree

// 3.1

// Type inference for "f":
// The type of "f" is: ('a * 'a tree) -> 'a tree
// Firstly, in the match-expression, the constructors for 'tree'
// are used, so we know that "t" is a tree type.
// In the if-statement, we observer that (>) is applied to "n" and 0,
// and since 0 is of type int, and (>): 'a -> 'a -> bool when 'a: comparison,
// we can conclude that "n" must be of type int.
// As for the return type, no functions are applied to the value "a" of Br, which means that if we assume that t: 'a tree,
// the type of "a" doesn't change, so "f" returns 'a tree.

// What does "f" do?
// "f" takes the first "n" levels of a tree, if the tree is deep enough.
// Otherwise, it just returns a leaf if the tree is not deep enough.
let rec f (n, t) =
  match t with
    | Lf -> Lf
    | Br (a, t1, t2) ->
      if n > 0 then
        Br (a, f (n - 1, t1), f (n - 1, t2))
      else
        Lf;

// Type inference for "g":
// The type of "g" is: ('a -> bool) -> 'a tree -> 'a tree
// We observe that constructors of the tree type are used
// in the 'function'-expression meaning that g takes two
// parameters, where the second is implicit. Let us call
// the second implicit parameter: "t". We know that "t" has
// the type: 'a tree. We also know that "p" must be a function
// since it is applied to "a" in a when-clause, and a when-clause
// can only work with booleans, we conclude that "p" must have the
// type: 'a -> bool.
// Similar to "f", no functions are applied to the "a" of the "Br"-constructor,
// So the return type of "g" is the same as "t", which is: 'a tree.

// What does "g" do?
// It works kinda like a filter function; that is, every branch
// in a tree, whose value "a" satisfies some predicate "p", will
// be returned. In all other cases, a leaf "Lf" is returned.
let rec g p = function
  | Br (a, t1, t2) when p a -> Br (a, g p t1, g p t2)
  | _ -> Lf

// Type inference for "h":
// The type of "h" is: ('a -> 'b) -> 'a tree -> 'b tree
// Due to the "function"-expression, we know that "h" takes
// a second implicit parameter. Let us call this "t".
// Since constructors of the tree type appear in the patterns,
// we know that "t" must have the type: 'a tree.
// "k" must be a function, since it is applied to "a" of a "Br",
// and "a" has the type: 'a. We cannot infer any further constraints about
// the return type of "k", we must assume that is has the most generic type: 'b,
// and thus "k" has the type: 'a -> 'b. 
// Finally, this means that "h" must have the return type: 'b tree.

// What does "h" do?
// "h" is basically the "map"-function implemented for the 'a tree type,
// since it applies some generic unary function to every "a" of a "Br" in the tree.
// (this means that the 'a tree type is actually a functor).
let rec h k = function
  | Lf -> Lf
  | Br (a, t1, t2) -> Br (k a, h k t1, h k t2)


// Problem 4
let rec map f = function
  | [] -> []
  | x::xs -> f x :: map f xs

let rec rev = function
  | [] -> []
  | x::xs -> rev xs @ [x]

[<EntryPoint>]
let main args =
  0
