
// Problem 1
let rec f n = function
  | 0 -> 1
  | k when k > 0 -> n * (f n (k - 1))
  | _ -> failwith "illegal argument"

let rec g p f = function
  | [] -> []
  | x::xs when p x -> f x :: g p f xs
  | _::xs -> g p f xs

type T =
  | A of int
  | B of string
  | C of T * T

let rec h = function
  | A n -> string n
  | B s -> s
  | C (t1, t2) -> h t1 + h t2

let sq = Seq.initInfinite (fun i -> 3*i)

let k j = seq { for i in sq do
                   yield (i, i - j)}

let xs = Seq.toList (Seq.take 4 sq)
let ys = Seq.toList (Seq.take 4 (k 2))

// 1.1
// f 5 3 ==> 125
// g ((/=) 3) ((*) 2) [1; 2; 3; 4; 5] ==> [2; 4; 3; 8; 10]
// h C (A 3, B "foo") ==> "3foo"

// 1.2
// f: int -> int -> int
// g: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list 
// h: T -> string

// 1.3
// Accumulating parameter
let f2 n k =
  let rec fA acc n = function
    | 0 -> acc
    | k when k > 0 -> fA (n * acc) n (k - 1)
    | _ -> failwith "illegal argument"

  fA 1 n k

// Continuation-based
let f3 n k =
  let rec fC c n = function
    | 0 -> c 1
    | k when k > 0 -> fC (fun res -> n * res) n (k - 1)
    | _ -> failwith "illegal argument"

  fC id n k

// 1.4
// sq: Seq<int>
// k: Seq<int * int>
// sq computes the sequence 0, 3, 6, 9, 12..., i.e. multiples of 3
// k computes a sequence of tuples (pairs) based on sq, where the second element
// has a negative offset of j.

// 1.5
// xs ==> [0; 3; 6; 9]
// ys ==> [(0, -2); (3, 1); (6, 4); (9, 7)]


// Problem 2

// 2.1
let ordered: int list -> bool = List.pairwise >> List.forall (fun (a, b) -> a <= b)

// 2.2
let smallerThanAll x xs = ordered (x::xs)

// 2.3
let rec insertBefore p x = function
  | [] -> [x]
  | x'::xs when p x' -> x::x'::xs
  | _::xs -> insertBefore p x xs

// 2.4
type Sex =
  | M
  | F

let sexToString = function
  | M -> "Male"
  | F -> "Female"

// 2.5
let replicate n str =
  let rec replicateC c str = function
    | 0 -> c ""
    | n when n > 0 -> replicateC (fun res -> str + res) str (n - 1) 
    | _ -> failwith "illegal argument"

  replicateC id str n


// Problem 3
type Name = string
type YearOfBirth = int

type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list

// 3.1
let rec isWF (P (n, s, yb, cs)): bool =
  let rec isEldest ch = List.forall (fun (P (_, _, y, c)) -> yb > y && isEldest c) ch
  let childrenOrdered = List.map (fun (P (_, _, y, _)) -> y) cs |> List.rev |> ordered
  isEldest cs && childrenOrdered

// 3.2
let makePerson (n, s, yb) = P (n, s, yb, [])

// 3.3
//let rec insertChildOf n c t =
//and insertChildOfInList n c cs = 


// 3.4
let rec extract = function
  | [] -> []
  | P (n, _, _, cs')::cs -> 
    n::extract cs'@extract cs

let rec find n = function
  | P (n', s, yb, cs) -> 
    if n = n' then 
      Some (s, yb, extract cs) 
    else
      searchChildren n cs
and searchChildren n = function
  | [] -> None
  | c::cs -> match find n c with
             | None -> searchChildren n cs
             | t -> t

[<EntryPoint>]
let main args =
  0
