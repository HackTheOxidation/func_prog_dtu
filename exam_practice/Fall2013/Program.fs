
open System

module Multiset =
  type Multiset<'a when 'a: equality> = ('a * int) list

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
      
  let insert e n ms =
    if contains e ms then
      ms
      |> List.map (fun (x, i) -> if x = e then (x, i + n) else (x, i))
    else
      (e, n)::ms

  let numberOf e ms =
    ms
    |> List.find (fst >> ((=) e))
    |> snd

  let delete e = List.filter (fst >> ((<>) e))

  let union ms1 ms2 =
    ms1
    |> List.fold (fun acc (e, n) -> insert e n acc) ms2

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
  


[<EntryPoint>]
let main args =
  0
