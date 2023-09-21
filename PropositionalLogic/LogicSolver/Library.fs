namespace LogicSolver

module Propositional =

    type Prop =
        | A of string
        | Dis of Prop * Prop
        | Con of Prop * Prop
        | Neg of Prop

    let rec negationNormalForm = function
        | Neg (Dis (l, r)) ->
          Con (negationNormalForm (Neg l), negationNormalForm (Neg r))
        | Neg (Con (l, r)) ->
          Dis (negationNormalForm (Neg l), negationNormalForm (Neg r))
        | Neg(Neg a) ->
          negationNormalForm a
        | a -> a

    let rec disjuctiveNormalForm = function
        | Con (a, Dis (b, c)) ->
          Dis (disjuctiveNormalForm <| Con (a, b), disjuctiveNormalForm <| Con (a, c))
        | Con (Dis (a, b), c) ->
          Dis (disjuctiveNormalForm <| Con (a, c), disjuctiveNormalForm <| Con (b, c))
        | a -> a
