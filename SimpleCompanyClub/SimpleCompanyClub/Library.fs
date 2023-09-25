namespace SimpleCompanyClub

module SimpleCompanyClub =
  type Member =
    { Name: string;
      Description: int * int * Set<string> }

  type Register = Member list
  
  let p1 { Name=_; Description=(_, yb, ths)} =
    let interests = Set.empty.Add("soccer").Add("jazz")
    yb > 1982 && Set.isSubset interests ths

  let p2 { Name=_; Description=(_, yb, ths)} =
    let interests = Set.empty.Add("soccer").Add("jazz")

    ths
    |> Set.intersect interests
    |> Set.isEmpty
    |> not
    |> (&&) (yb > 1982)
    
  let extractTargetGroup p r =
    r
    |> List.filter p
    |> List.map (fun { Name=name; Description=(no, _, _)} -> (name, no))
