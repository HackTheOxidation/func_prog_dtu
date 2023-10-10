namespace HR

module HR4 =
  let count (xs: int list, x: int): int =
    xs
    |> List.filter ((=) x)
    |> List.length

  let insert (xs: int list, x: int): int list =
    let index = List.findIndex ((<) x) xs
    List.insertAt index x xs

  let intersect (pair: int list * int list): int list =
    let rec traverse acc = function
      | (x::xs, x'::xs') ->
        if x = x' then
          traverse (acc @ [x]) (xs, xs')
        elif x < x' then
          traverse acc (xs, x'::xs')
        else
          traverse acc (x::xs, xs')
      | _ -> acc

    pair
    |> traverse []

  let plus (pair: int list * int list): int list =
    let rec plusInner acc = function
      | ([], []) -> acc
      | ([], xs') -> acc @ xs'
      | (xs, []) -> acc @ xs
      | (x::xs, x'::xs') ->
        if x = x' then
          plusInner (acc @ [x; x']) (xs, xs')
        elif x < x' then
          plusInner (acc @ [x]) (xs, x'::xs')
        else
          plusInner (acc @ [x']) (x::xs, x'::xs')

    pair
    |> plusInner []
    
  let minus (pair: int list * int list): int list =
    let rec minusInner acc = function
      | ([], []) | ([], _) -> acc
      | (xs, []) -> acc @ xs
      | (x::xs, x'::xs') ->
        if x = x' then
          minusInner acc (xs, xs')
        elif x < x' then
          minusInner (acc @ [x]) (xs, x'::xs')
        else
          minusInner acc (x::xs, xs')

    pair
    |> minusInner []
