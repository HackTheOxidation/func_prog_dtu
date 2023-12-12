
// Problem 1
type Name = string
type Score = int
type Result = Name * Score

// 1.1
let legalResults: (Result list -> bool) = List.forall (fun (_, s) -> 0 <= s && s <= 100)

// 1.2
let maxScore: (Result list -> int) = List.maxBy snd >> snd

// 1.3
let best = function
  | [] -> failwith "Empty list"
  | r::rs -> List.fold (fun (n', s') (n, s) -> if s > s' then (n, s) else (n', s')) r rs

// 1.4
let average: (Result list -> float) = function
  | [] -> 0.0
  | rs -> 
    let sum = List.map snd >> List.fold (+) 0 >> float
    let len = List.length >> float
    sum rs / len rs

// 1.5
let delete r rs = 
  let rec deleteC c r = function
    | [] -> c []
    | r'::rs -> 
      if r' = r then 
        deleteC c r rs 
      else 
        deleteC (fun res -> r'::res) r rs

  deleteC id r rs

// Using List.take will throw either an ArgumentException if 'rs' is empty
// or an InvalidOperationException if 'rs' contains fewer than 'n' elements.
let bestN rs n =
  rs
  |> List.sortBy snd
  |> List.take n


// Problem 2
type Typ =
  | Integer
  | Boolean
  | Ft of Typ list * Typ

type Decl = string * Typ

// 2.1
let distinctVars ds =
  let rec distinctVarsC acc = function
    | [] -> true
    | d::ds -> if Set.contains d acc then false else distinctVarsC (Set.add d acc) ds

  distinctVarsC Set.empty ds

// Alternative definition using pipes
let distinctVars2 ds =
  ds
  |> Set.ofList
  |> Set.count
  |> (=) (List.length ds)

type SymbolTable = Map<string, Typ>

// 2.2
let toSymbolTable: (Decl list -> SymbolTable) = Map.ofList

// Map.add overrides the value of existing keys in case of collision.
let rec toSymbolTableC tb = function
  | [] -> tb
  | (s, t)::ds -> toSymbolTableC (Map.add s t tb) ds

// Alternative definition using accumulator.
let toSymbolTable2: (Decl list -> SymbolTable) = toSymbolTableC Map.empty 

// 2.3
let extendST (tb: SymbolTable): (Decl list -> SymbolTable) = toSymbolTableC tb

type Exp =
  | V of string
  | A of string * Exp list

// 2.4
let symbolsDefined sym e: bool =
  let rec symbolsDefinedC sym: (Exp -> bool) = function
    | V v -> Map.containsKey v sym
    | A (f, es) -> 
      if Map.containsKey f sym then
        List.forall (symbolsDefinedC sym) es
      else
        false

  symbolsDefinedC sym e

// 2.5

exception InvalidType of Typ

let rec checkType t sym e =
  match (t, e) with
    | (t, V v) -> Map.find v sym = t
    | (Ft (args, rt), A (f, es)) ->
      let t = Map.find f sym
      Ft (args, rt) = t && List.zip args es |> List.forall (fun (at, ae) -> checkType at sym ae)
    | _ -> false

let typOf sym = function
  | V v -> Map.find v sym
  | A (f, es) ->
    match Map.find f sym with
      | Ft (args, rt) ->
        if List.zip args es |> List.forall (fun (at, ae) -> checkType at sym ae) then
          Ft (args, rt)
        else
          raise (InvalidType <| Ft (args, rt))
      | t -> raise (InvalidType t)

type Stm =
  | Ass of string * Exp
  | Seq of Stm * Stm
  | Ite of Exp * Stm * Stm
  | While of Exp * Stm
  | Block of Decl list * Stm

// 2.6
let rec wellTyped sym = function
  | Ass (x, e) -> 
    try
      Map.find x sym = typOf sym e
    with
    | :? System.Collections.Generic.KeyNotFoundException -> false
    | :? InvalidType -> false
  | Seq (stm1, stm2) -> wellTyped sym stm1 && wellTyped sym stm2
  | Ite (e, stm1, stm2) ->
    try
      let t = typOf sym e 
      t = Boolean && wellTyped sym stm1 && wellTyped sym stm2
    with
    | :? System.Collections.Generic.KeyNotFoundException -> false
    | :? InvalidType -> false
  | While (e, stm) ->
    try
      let t = typOf sym e 
      t = Boolean && wellTyped sym stm
    with
    | :? System.Collections.Generic.KeyNotFoundException -> false
    | :? InvalidType -> false
  | Block (decls, stm) ->
    distinctVars decls && wellTyped (extendST sym decls) stm


// Problem 3 

// 3.1
// The type of "h" is: 'a list -> 'a list -> 'a list
let rec h a b =
  match a with
    | [] -> b
    | c::d -> c::(h d b)

type T<'a, 'b> =
  | A of 'a
  | B of 'b
  | C of T<'a, 'b> * T<'a, 'b>

// 3.2

[<EntryPoint>]
let main args =
  0
