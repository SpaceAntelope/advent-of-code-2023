#load "./lava-common.fsx"

open ``Lava-common``

open System
open System.IO

"./puzzle.input"
|> File.ReadAllText
|> parse
|> tripTheLight ToRight (0,0)
|> countEnergized
|> printfn "Counted %d energized tiles." 