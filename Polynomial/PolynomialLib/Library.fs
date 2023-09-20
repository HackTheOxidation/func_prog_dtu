namespace PolynomialLib

module Polynomial =

  // Type definition for polynomials
  type Polynomial = int list

  // Part 1

  (* Auxillary function that combines two lists of varying lengths
  by applying 'op' while there are elements in both and then applying
  'opTail' on the remaining elements of the longer list. *)
  let rec zipWith op opTail p1 p2 =
    match (p1, p2) with
      | ([], ps) | (ps, []) -> List.map opTail ps
      | (x::xs, y::ys) -> op x y :: zipWith op opTail xs ys

  let add p1 p2 =
    zipWith (+) id p1 p2

  let mulC m p =
    List.map ((*) m) p

  let sub p1 p2 =
    zipWith (-) ((*) -1) p1 p2

  let mulX p =
    0 :: p

  let divX = function
    | [] -> []
    | x::xs -> xs

  // Uber-complicated polynomial multiplication.
  let mul (p1: Polynomial) (p2: Polynomial): Polynomial =
    // Annotate each element with its exponent (index).
    let idx (p: Polynomial) =
      List.mapi (fun i a -> (a, i)) p

    // Consecutively apply mulX 'i' times on 'p'.
    let rec mulXs i (p: Polynomial): Polynomial =
      if i > 0 then mulX p |> mulXs (i - 1)
      else p
        
    // Pair-wise multiplication
    let pairMul (a1, i1) (a2, i2) =
      (a1 * a2, i1 * i2)

    zipWith pairMul id (idx p1) (idx p2)
    |> List.map (fun (a, i) -> mulXs i [a])
    |> List.reduce add

  // The less complicated version of polynomial multiplication.
  let rec mul2 p1 p2 =
    match p1 with
      | [] -> []
      | x::xs -> add (mulC x p2) (mul2 xs p2)
  
  let eval (x: int) p =
    let rec pow x i =
      if i > 0 then x * pow x (i - 1)
      else 1

    List.mapi (fun i a -> a * pow x i) p
    |> List.reduce (+)

  // Part 2

  // Assert that an integer list is a legal representation of a polynomial.
  let rec isLegal p =
    match p with
      | [] -> true
      | [x] -> x <> 0
      | _::xs -> isLegal xs

  let ofList l =
    l
    |> List.rev
    |> List.skipWhile ((=) 0)
    |> List.rev


  let toString = function
      | [] -> "0"
      | p ->
        List.mapi (fun i a -> $"{a}x^{i}") p
        |> String.concat " + "

  let derivative p =
    p
    |> List.mapi (fun i a -> i * a)
    |> divX

  // Part 3

    (* Determine which of the definitions from Part 1
    (and maybe Part 2) that doesn't preserve the invariant
    "isLegal" and fix it. *)

  // Part 4

  type Degree =
    | MinusInf
    | Fin of int

  let degree = function
    | [] -> MinusInf
    | p -> Fin <| List.length p - 1
