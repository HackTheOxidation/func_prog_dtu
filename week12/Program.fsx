
// Problem 1, May 2018
let rec f xs ys = 
  match (xs, ys) with
  | (x::xs1, y::ys1) -> x::y::f xs1 ys1
  | _ -> []

// 1.1
// f [1; 6; 0; 8] [0; 7; 3; 3] 
//   ==> 1::0::6::7 f [0; 8] [3; 3]
//   ==> 1::0::6::7::0::3 f [8] [3]
//   ==> 1::0::6::7::0::3::8::3 f [] []
//   ==> 1::0::6::7::0::3::8::3::[]
//   ==> [1; 0; 6; 7; 0; 3; 8; 3]

// 1.2
// Type of f: 'a list -> 'a list -> 'a list
// "f" merges two lists to a single list as long as both lists have values in them.

// 1.3
// "f" is not tail recursive since the (::)-operator is applied after the recursive call to "f".
// Accumulating parameter-based implementation:
let f2 xs ys =
  let fA acc xs ys =
    match (xs, ys) with
    | (x::xs1, y::ys1) -> f (acc@[x; y]) xs1 ys1
    | _ -> acc

  fA [] xs ys

// 1.4
// Continuation-based implementation:
let f3 xs ys =
  let fC c xs ys =
    match (xs, ys) with
    | (x::xs1, y::ys1) -> fC (fun res -> x::y::res) xs1 ys2
    | _ -> c []

  fC id xs ys


// Problem 2.1, May 2017
let rec f = function
  | 0 -> [0]
  | i when i > 0 -> i::g(i - 1)
  | _ -> failwith "Negative argument"
and g = function
  | 0 -> []
  | n -> f(n - 1)

let h s k = seq { for a in s do
                        yield k a }

// 1.1
// Value of: f 5 ==> [5; 3; 1]
// Value of: h (seq [1; 2; 3; 4]) (fun i -> i + 10) ==> seq [11; 12; 13; 14]

// Type of f: int -> int list
// Type of h: 'a seq -> ('a -> 'b) -> 'b seq

// "f" computes a list of descending (by 2) natural numbers. 
// If the argument is odd, "f" returns a list of odd numbers otherwise it returns a list of even numbers.
// "h" takes a sequence "s" and applies a function "k" to each of its elements, 
// so this is an implementation of the "map"-function for sequences.


// Problem 3, May 2016
type Container =
  | Tank of float * float * float // (length, width, height)
  | Ball of float                 // radius

// 1.1
let tank = Tank (3.2, 7.1, 10.6)
let ball = Ball 6.8

// 1.2
let isWF = function
  | Tank (l, w, h) -> l > 0 && w > 0 && h > 0
  | Ball r -> r > 0

// 1.3
let volume = function
  | Tank (l, w, h) -> l * w * h
  | Ball r -> (4.0/3.0) * System.Math.PI * r ** 3

// 1.4
type Container =
  | Tank of float * float * float // (length, width, height)
  | Ball of float                 // radius
  | Cylinder of float * float     // (radius, height)

let isWF = function
  | Tank (l, w, h) -> l > 0 && w > 0 && h > 0
  | Ball r -> r > 0
  | Cylinder (r, h) -> r > 0 && h > 0

let volume = function
  | Tank (l, w, h) -> l * w * h
  | Ball r -> (4.0/3.0) * System.Math.PI * r ** 3
  | Cylinder (r, h) -> System.Math.PI * r ** 2 * h

// 1.5
type Name = string
type Contents = string
type Storage = Map<Name, Contents * Container>

let store = Storage Map ["tank1", ("oil", tank); "ball1", ("water", ball)]

// 1.6
let find n stg =
  stg |> Map.find n |> (fun (cnt, c) -> (cnt, volume c))
  

// Problem 4, May 2016
type T<'a> = 
  | L 
  | N of T<'a> * 'a * T<'a>

let rec f g t1 t2 =
  match (t1, t2) with
  | (L, L) -> L
  | (N (ta1, va, ta2), N (tb1, vb, tb2)) -> N (f g ta1 tb1, g (va, vb), f g ta2 tb2)

let rec h t =
  match t with
  | L -> L
  | N (t1, v, t2) -> N(h t2, v, h t1)

let rec g = function
  | (_, L) -> None
  | (p, N (t1, a, t2)) with p a -> Some (t1, t2)
  | (p, N (t1, a, t2)) -> 
    match g (p, t1) with
    | None -> g (p, t2)
    | res -> res

let t = N (N (L, 1, N (N (L, 2, L), 1, L)), 3, L)

// 4.1
// Type of t: T<int>
let tb1: T<bool list> = L
let tb2 = N (L, [true; false;], L)
let tb3 = N (N (L, [true], L), [false], N (L, [true; false], L))

// 4.2
// Type of f: ('a * 'b -> 'c) -> T<'a> -> T<'b> -> T<'c>
// Type of h: T<'a> -> T<'a>
// Type of g: ('a -> bool) * T<'a> -> (T<'a> * T<'a>) option

// "f" computes a new tree from two trees by applying a function "g"
// to transform the values of the old trees into a new one.
// Note, that the two trees "t1" and "t2" must have the same overall structure
// for "f" to work, otherwise "f" is undefined for ex. (L, N _)

// "h" computes the mirror of a tree "t" (T<'a>), that is for some tree N (a, v, b),
// "h" returns the mirror N (b, v, a)

// "g" finds a pair of sub-trees (wrapped in an option monad) if some node in a tree 
// satisfies some predicate function "p". If no node satisfies the predicate, then None is returned.

// 4.3
// Continuation-based implementation:
let count a t =
  let countC c a = function
    | L -> c 0
    | N (t1, a', t2) when a = a' -> countC (fun l -> countC  (fun r -> 1 + l + r) a t2) a t1
    | N (t1, _, t2) -> countC (fun l -> countC (fun r -> l + r) a t2) a t1

  countC id a t

// 4.4
// Continuation-based implementation:
let replace a b t = 
  let rec replaceC c a b = function
  | L -> c L
  | N (t1, a', t2) ->
    let v = if a = a' then b else a'
    replaceC (fun l -> replaceC (fun r -> N (l, v, r)) a b t2) a b t1

  replaceC id a b t


