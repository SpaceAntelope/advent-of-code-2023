#load "./pipes-common.fsx"

open System.IO
open ``Pipes-common``

File.Delete("./output.txt")

"./puzzle.input"
|> File.ReadAllText
|> parse
|> findFurthestDistance
|> printfn "Furthest distance: %d"