
module Compiler =

  type Instruction =
    | ADD
    | SUB
    | SIGN
    | ABS
    | PUSH of int

  type Stack = int list

  let intpInstr stack instr =
    match (stack, instr) with
      | (a::b::rest, ADD) -> (b + a)::rest
      | (a::b::rest, SUB) -> (b - a)::rest
      | (a::rest, SIGN) -> (- a)::rest
      | (_, PUSH r) -> r::stack
      | _ -> stack

  let exec (instrs: Instruction list): int =
    instrs
    |> List.fold intpInstr []
    |> List.item 0

  type Exp =
    | X
    | C of int
    | Abs of Exp
    | Minus of Exp
    | Add of Exp * Exp
    | Sub of Exp * Exp

  let rec sem e x =
    match e with
      | X -> x
      | C c -> c
      | Abs exp -> abs <| sem exp x
      | Minus exp -> -(sem exp x)
      | Add (l, r) -> (sem l x) + (sem r x)
      | Sub (l, r) -> (sem l x) - (sem r x)

  let compile (e: Exp) (x: int): Instruction list =
    let rec compileC (stack: Instruction list) (e: Exp) (x: int): Instruction list =
      match e with
        | X -> (PUSH x)::stack
        | C c -> (PUSH c)::stack
        | Abs exp -> compileC (ABS::stack) exp x
        | Add (l, r) ->
          let left = compileC [] l x
          let right = compileC [] r x
          left @ right @ (ADD::stack)
        | Sub (l, r) ->
          let left = compileC [] l x
          let right = compileC [] r x
          left @ right @ (SUB::stack)
        | _ -> stack
     
    compileC [] e x


[<EntryPoint>]
let main args =
  0
