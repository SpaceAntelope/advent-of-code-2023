#load "./gear-ratios-common.fsx"

open System.IO
open ``Gear-ratios-common``

File.ReadAllText("./puzzle.input")
|> parse
|> gearRatios
|> List.sum
|> printfn "The sum of all gear ratios is %A"