
type Book = string
type Shelf = Book list

type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";
           "To mock a mockingbird";
           "What is the name of this book"]

let ls0 = [("Communication and concurrency", "Bob", 4);
           ("Programming in Haskell", "Paul", 2);
           ("Communicating Sequential processes", "Mary", 7);
           ("Elements of the theory of computation", "Dick", 1)]

let rec onShelf book = function
  | [] -> false
  | x::xs ->
    if x = book then
      true
    else
      onShelf book xs

let toShelf b bs = 
  let rec toShelfC c b = function
    | [] -> c [b]
    | x::xs ->
      if b < x then
        c (b::x::xs)
      else
        toShelfC (fun res -> c (x::res)) b xs

  toShelfC id b bs

let fromShelf b bs = 
  let rec fromShelfC c b = function
    | [] -> None
    | x::xs ->
      if b = x then
        Some <| c xs
      else
        fromShelfC (fun res -> c (x::res)) b xs

  fromShelfC id b bs

let addLoan b n d ls = 
  let rec addLoanC c b n d = function
    | [] -> c [(b, n, d)]
    | x::xs ->
      let (b', _, _) = x
      if b < b' then
        c ((b, n, d)::x::xs)
      else
        addLoanC (fun res -> c (x::res)) b n d xs

  addLoanC id b n d ls
      
let removeLoan b n ls = 
  let rec removeLoanC b n c = function
    | [] -> c []
    | x::xs ->
      let (b', n', _) = x
      if b = b' && n = n' then
        c xs
      else
        removeLoanC b n (fun res -> c (x::res)) xs
  removeLoanC b n id ls

let reminders d ls =
  let rec remindersC c d = function
    | [] -> c []
    | x::xs ->
      let (_, _, d') = x
      if d' < d then
        remindersC (fun res -> c(x::res)) d xs
      else
        remindersC c d xs

  remindersC id d ls

let toLetters ls = 
  let rec toLettersC c = function
    | [] -> c []
    | (n, b)::xs ->
      let (title, name, _) = b
      toLettersC (fun res -> c ($"\"Dear {name}!\nPlease return \"{title}\".\nRegards {n}\""::res)) xs

  toLettersC id ls

let toLetters' ls =
  ls
  |> List.map (fun (n, b) ->
               let (title, name, _) = b
               $"\"Dear {name}!\nPlease return \"{title}\".\nRegards {n}\"")

let reminders' d0 ls =
  ls
  |> List.foldBack (fun (b, n, d) acc ->
                    if d < d0 then (b, n, d)::acc
                    else acc) []

[<EntryPoint>]
let main args =
  0
