namespace MergeSort

module MergeSort =
  let rec merge xs ys =
    match (xs, ys) with
      | ([], []) -> []
      | ([], ys) -> ys
      | (xs, []) -> xs
      | (x::xs, y::ys) ->
        if x < y then
          x :: merge xs (y::ys)
        else
          y :: merge (x::xs) ys

  let rec split = function
    | [] -> ([], [])
    | [x] -> ([x],[])
    | x1::x2::xs ->
      let (xl, xr) = split xs
      (x1::xl, x2::xr)

  let rec splitC xs c =
    match xs with
      | [] -> c ([], [])
      | [x] -> c ([x], [])
      | x1::x2::xs ->
        splitC xs (fun (xl, xr) -> c (x1::xl, x2::xr))
    
  let rec sort = function
    | [] -> []
    | [x] -> [x]
    | xs ->
      let (l, r) = split xs
      merge (sort l) (sort r)

  let rec sortC xs c =
    match xs with
      | [] -> c []
      | [x] -> c [x]
      | xs ->
        let (xl, xr) = splitC xs id
        sortC xl (fun vl -> sortC xr (fun vr -> c <| merge vl vr)) 
    
