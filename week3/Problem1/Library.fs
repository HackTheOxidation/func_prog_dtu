namespace Problem1

module Problem1 =

  let numberOf x ys =
    ys
    |> List.filter ((=) x)
    |> List.length

  let positionsOf x ys =
    ys
    |> List.indexed
    |> List.filter (fun (i, y) -> x = y)
    |> List.map (fun (i, _) -> i)

  // The easy version.
  let filterMap p f xs =
    xs
    |> List.filter p
    |> List.map f

  // The hand-written, wheel reinventing version.
  let filterMap2 p f xs =
    let rec filter p xs =
      match xs with
        | [] -> []
        | x::xs ->
          if p x then
            x :: filter p xs
          else
            filter p xs

    let rec map f xs =
      match xs with
        | [] -> []
        | x::xs -> f x :: map f xs
    
    xs
    |> filter p
    |> map f
