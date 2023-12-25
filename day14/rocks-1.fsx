#load "./rocks-common.fsx"

open System
open System.IO
open System.Collections.Generic
open ``Rocks-common``

"./puzzle.input"
|> File.ReadAllText
|> parse
|> tiltNorth
|> score
|> printfn "Total load is %d"