open System.Collections.Generic

module Fall2015 =
  type Appliance = string
  type Usage = Appliance * int

  let inv ats = List.forall (snd >> ((<=) 0)) ats

  let durationOf a ats =
    ats
    |> List.filter ((=) a)
    |> List.fold (fun acc u -> acc + snd u) 0

  let isWellFormed ats =
    ats
    |> List.fold (fun acc u -> acc + snd u) 0
    |> (>=) 24
    |> (&&) (inv ats)

  let delete (a: Appliance) (ats: Usage list): Usage list =
    ats
    |> List.filter (fst >> ((<>) a))

  type Price = int
  type Tariff = Map<Appliance, Price>

  let isDefined (ats: Usage list) (trf: Tariff): bool =
    ats
    |> List.forall (fst >> trf.ContainsKey)

  exception UndefinedApplianceError of Appliance

  let priceOf (ats: Usage list) (trf: Tariff): Price =
    let tryAcc acc u =
      let a = fst u
      try
        Map.find a trf |> (+) acc
      with
        | :? KeyNotFoundException -> raise (UndefinedApplianceError(a))

    ats
    |> List.fold tryAcc 0

let main argv =
  0
