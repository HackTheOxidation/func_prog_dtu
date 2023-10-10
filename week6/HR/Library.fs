namespace HR

module HR5 =
  let (+?) (p: int -> bool) (s: int) (v: int): int =
    if p v then
      s + v
    else
      s

  let sum (p: int -> bool) =
    List.fold ((+?) p) 0
