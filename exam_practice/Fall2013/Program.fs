
open System

// Problem 1
module Multiset =
  type Multiset<'a when 'a: equality> = ('a * int) list

  // 1.1
  let elements (ms: Multiset<'a>) =
    ms
    |> List.map fst
    |> List.distinct

  let inv ms =
    ms
    |> elements
    |> List.length
    |> (=) (List.length ms)
    
  let contains e =
    elements >> List.contains e
      
  // 1.2
  let insert e n ms =
    if contains e ms then
      ms
      |> List.map (fun (x, i) -> if x = e then (x, i + n) else (x, i))
    else
      (e, n)::ms

  // 1.3
  let numberOf e ms =
    ms
    |> List.find (fst >> ((=) e))
    |> snd

  // 1.4
  let delete e = List.filter (fst >> ((<>) e))

  // 1.5
  let union ms1 ms2 =
    ms1
    |> List.fold (fun acc (e, n) -> insert e n acc) ms2


// 1.6
module MultisetMap =
  type MultisetMap<'a when 'a: comparison> = Map<'a, int>

  let inv ms = Map.forall (fun _ n -> n > 0) ms

  let insert e n =
    Map.change e (fun x ->
                  match x with
                  | Some v -> Some (v + n)
                  | None -> Some n)

  let union ms1 ms2 = 
    Map.fold (fun acc e n -> insert e n ms1) ms2
  

// Problem 2
type 'a Tree = 
  | Lf
  | Br of 'a Tree * 'a * 'a Tree

// 2.1
// Type: 'a list -> int -> (int * 'a) list
let rec f i = function
  | [] -> []
  | x::xs -> (i, x)::f (i*i) xs

// Type: ('a -> bool) -> 'a Tree -> 'a Tree option
let rec g p = function
  | Lf -> None
  | Br (_, a, t) when p a -> Some t
  | Br (t1, a, t2) -> 
    match g p t1 with
      | None -> g p t2
      | res -> res

// 2.2
// Accumulator-based
let f2 i xs =
  let rec fA acc i = function
    | [] -> acc
    | x::xs -> fA (acc@[(i, x)]) (i*i) xs

  fA [] i xs

// Continuation-based
let f3 i xs =
  let rec fC c i = function
    | [] -> c []
    | x::xs -> fC (fun res -> (i, x)::res) (i * i) xs

  fC id i xs

// Preference: Continuation-based approach is more efficient.

// 2.3

// h (*) (4, 1) 
// ==> h (*) (3, 4) 
// ==> h (*) (2, 12) 
// ==> h (*) (1, 24) 
// ==> h (*) (0, 24)
// ==> 24

// Type: (int -> 'a -> 'a) -> int * 'a -> 'a
// "h" applies a binary function "f" on some input "e" "n" times.
let rec h f (n,e) = 
  match n with
  | 0 -> e
  | _ -> h f (n - 1, f n e)

// 2.4
// Type: Seq<int>
let A = Seq.initInfinite id;;

// Type: Seq<int * int>
let B = seq { for i in A do
                  for j in seq {0 .. i} do
                        yield (i, j) }

// Type: Seq<int * int>
let C = seq { for i in A do
                  for j in seq {0 .. i} do
                        yield (i-j, j) }

// X = [0; 1; 2; 3]
let X = Seq.toList (Seq.take 4 A)
// Y = [(0, 0); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2)]
let Y = Seq.toList (Seq.take 6 B)
// Z = [(0, 0); (1, 0); (0, 1); (2, 0); (1, 1); (0, 2)]
let Z = Seq.toList (Seq.take 10 C)

// A gives an infinite sequence of natural numbers [0;infty[
// B gives an infinite sequence of (int * int) tuples, where
// C gives an infinite sequence of (int * int) tuples


// Problem 3
type Title = string

type Section = Title * Elem list
and Elem = Par of string | Sub of Section

type Chapter = Title * Section list
type Book = Chapter list

// 3.1
let maxL l =
  let rec maxLA acc = function
    | [] -> acc
    | x::xs -> if x > acc then maxLA x xs else maxLA acc xs

  maxLA 0 l

// 3.2
let overview: Book -> Title list = List.map fst

// 3.3
let rec depthSection (_, es) = 
  es |> List.map depthElem |> maxL |> (+) 1 
and depthElem = function
  | Par _ -> 0
  | Sub s -> depthSection s

let depthChapter (_, ss) =
  ss |> List.map depthSection |> maxL |> (+) 1

let depthBook: Book -> int = List.map depthChapter >> maxL

// 3.4
type Numbering = int list
type Entry = Numbering * Title
type Toc = Entry list

let rec tocE (is: int list): Elem list -> Toc = 
  List.mapi (fun i (e) -> match e with
                          | Par _ -> []
                          | Sub (t, es') -> (is@[i], t)::tocE (is@[i]) es') 
  >> List.concat

let rec tocS (is: int list): Section list -> Toc = 
  List.mapi (fun i (t, es) -> (is@[i], t)::tocE (is@[i]) es) >> List.concat

let rec tocB: Book -> Toc = 
  List.mapi (fun i (t, ss) -> ([i], t)::tocS [i] ss) >> List.concat

[<EntryPoint>]
let main args =
  0
