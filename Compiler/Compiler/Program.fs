
module Compiler =

  // Types of low-level instructions (assembly-like)
  type Instruction =
    | ADD
    | SUB
    | SIGN
    | ABS
    | PUSH of int

  // A stack is just a list of values (ints)
  type Stack = int list

  // Operations on the stack 
  let intpInstr stack instr =
    match (stack, instr) with
      | (a::b::rest, ADD) -> (b + a)::rest
      | (a::b::rest, SUB) -> (b - a)::rest
      | (a::rest, SIGN) -> (- a)::rest
      | (_, PUSH r) -> r::stack
      | _ -> stack

  // Executes a list of instructions using a stack machine
  let exec (instrs: Instruction list): int =
    instrs
    |> List.fold intpInstr []
    |> List.item 0

  // Expressions for an Abstract Syntax Tree.
  type Exp =
    | X
    | C of int
    | Abs of Exp
    | Minus of Exp
    | Add of Exp * Exp
    | Sub of Exp * Exp

  // Defines the semantics for each expression in the AST.
  let rec sem e x =
    match e with
      | X -> x
      | C c -> c
      | Abs exp -> abs <| sem exp x
      | Minus exp -> -(sem exp x)
      | Add (l, r) -> (sem l x) + (sem r x)
      | Sub (l, r) -> (sem l x) - (sem r x)

  // Translates an expression (AST) to a list of instructions (compilation).
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

  // Helper function for tail-recursive continuation-based implementation
  let rec compileC (stack: Instruction list) (e: Exp) (x: int) acc: Instruction list =
    match e with
      | X -> acc <| (PUSH x)::stack
      | C c -> acc <| (PUSH c)::stack
      | Abs exp -> compileC (ABS::stack) exp x acc
      | Add (l, r) ->
        compileC [] l x (fun left -> compileC [] r x (fun right -> acc (left@right@ADD::stack)))
      | Sub (l, r) ->
        compileC [] l x (fun left -> compileC [] r x (fun right -> acc (left@right@SUB::stack)))
      | _ -> acc stack

  let compileA e x = compileC [] e x id

  // Reduction (optimization) of ASTs.
  let rec red = function
    | Add (C i, C j) -> C (i + j)
    | Add (e, C 0) | Add (C 0, e) -> red e
    | Sub (C i, C j) -> C (i - j)
    | Sub (e, C 0) -> red e
    | Sub (C 0, e) -> red (Minus e)
    | Minus (C i) -> C (-i)
    | Minus (Minus e) -> red e
    | Abs (C i) -> C (abs i)
    | Abs (Minus e) -> red e
    | Abs (Abs e) -> red (Abs e)
    | e -> e

[<EntryPoint>]
let main args =
  0
