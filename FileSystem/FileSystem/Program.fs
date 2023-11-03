open System

[<AutoOpen>]
module FileSystem =
  type FileSys = Element list
  and Element =
    | File of string * string
    | Dir of string * FileSys

  let rec namesFileSys = function
    | [] -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
  and namesElement = function
    | File(name, extension) -> [name + "." + extension]
    | Dir(s, fs) -> s :: (namesFileSys fs)

  let rec searchFileSys ext = function
    | [] -> Set.empty
    | e::es -> Set.union (searchElement ext e) (searchFileSys ext es)
  and searchElement ext = function
    | File(name, extension) when extension = ext -> Set.singleton name
    | File _ -> Set.empty
    | Dir(_, fs) -> searchFileSys ext fs

  let rec longNamesFileSys = function
    | [] -> Set.empty
    | e::es -> Set.union (longNamesElement e) (longNamesFileSys es)
  and longNamesElement = function
    | File(s, ext) -> Set.singleton <| s + "." + ext
    | Dir(s, fs) ->
      let updatePath = function
        | File(name, extension) -> File(s + "\\" + name, extension)
        | Dir(name, fileSys) -> Dir(s + "\\" + name, fileSys)
      longNamesFileSys <| List.map updatePath fs

[<EntryPoint>]
let main args =
  let d1 = Dir("d1", [File("a1", "java");
                      Dir("d2", [File("a2","fsx");
                                 Dir("d3", [File("a3","fs")])]);
                      File("a4","fsx");
                      Dir("d3", [File("a5","pdf")])])

  printfn "names: %O" <| namesFileSys [d1]
  printfn "search fsx: %O" <| searchFileSys "fsx" [d1]
  printfn "long names: %O" <| longNamesFileSys [d1]
  0
