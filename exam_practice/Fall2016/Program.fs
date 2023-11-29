
module Problem1 =
  type Name = string
  type Event = string
  type Point = int
  type Score = Name * Event * Point

  type ScoreBoard = Score list

  let sb = [("Joe", "June Fishing", 35);
            ("Peter", "May Fishing", 30);
            ("Joe", "May Fishing", 28);
            ("Paul", "June Fishing", 28)]

  let inv scoreBoard =
    let allPos = List.forall ((<=) 0) scoreBoard
    let ordered =
      scoreBoard
      |> List.pairwise
      |> List.forall (fun (s1, s2) -> s1 >= s2)

    allPos && ordered

  let insert score scoreBoard =
    let (_, _, p) = score

    let rec insertC score = function
      | [] -> [score]
      | x::xs ->
        let (_, _, px) = x
        if px <= p then
          score::x::xs
        else
          x::(insertC score xs)

    if p < 0 then
      failwith "Score cannot have a negative number of points."
    else
      insertC score scoreBoard
  
  let get ((name, sb): Name * ScoreBoard): (Event * Point) list =
    sb
    |> List.filter (fun (n, _, _) -> n = name)
    |> List.map (fun (_, e, p) -> (e, p))

  let top k = function
    | [] -> None
    | sb when k < 0 || List.length sb < k -> None
    | sb -> Some <| List.take k sb

module Problem2 =
  // Type is 'a -> 'a -> 'a list -> 'a list
  let replace a b = List.map (fun e -> if e = a then b else e)

  // Tail recursive
  let replace2 a b xs =
    let rec replaceC c a b = function
      | [] -> []
      | x::xs ->
        if x = a then
          replaceC (fun res -> c(b::res)) a b xs
        else
          replaceC (fun res -> c(x::res)) a b xs
    replaceC id a b xs
    
module Problem3 =
  // pos: int seq
  let pos = Seq.initInfinite ((+) 1)
  // seq1: (int * int) seq
  let seq1 = seq { yield (0, 0)
                   for i in pos do
                     yield (i, i)
                     yield (-i, -i) }

  // val1: (int * int) seq
  // ==> { (0, 0); (1, 1); (-1, -1); (2, 2); (-2, -2)}
  let val1 = Seq.take 5 seq1

  // nat: int seq
  let nat = Seq.initInfinite id
  // seq2: (int * int) seq
  let seq2 = seq { for i in nat do
                      yield (i, 0)
                      for j in [1 .. i] do
                         yield (i,j) }

  // val2: (int * int) list
  // ==> [(0, 0); (1, 0); (1, 1); (2, 0); (2, 1);
  //      (2, 2); (3, 0); (3, 1); (3, 2); (3, 3)]
  let val2 = Seq.toList (Seq.take 10 seq2)


module Problem4 =
  type Tree<'a,'b> =
    | A of 'a
    | B of 'b
    | Node of Tree<'a, 'b> * Tree<'a, 'b>

  let rec countAs = function
    | A _ -> 1
    | B _ -> 0
    | Node (t1, t2) -> countAs t1 + countAs t2

  let rec subst a a' b b' = function
    | A v -> if a = v then A a' else A v
    | B v -> if b = v then B b' else B v
    | Node (t1, t2) ->
      let sub = subst a a' b b'
      Node (sub t1, sub t2)

  let rec g = function
    | Node (t1, t2) -> Node (g t2, g t1)
    | leaf -> leaf;;

  let rec f = function
    | A a -> ([a], [])
    | B b -> ([], [b])
    | Node (t1, t2) ->
      let (xs1, ys1) = f t1
      let (xs2, ys2) = f t2
      (xs1@xs2, ys1@ys2)
  

module Problem5 =
  type T<'a> = N of 'a * T<'a> list

  

[<EntryPoint>]
let main args =
  0
