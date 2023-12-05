#load "./gear-ratios-common.fsx"

open System.IO
open ``Gear-ratios-common``

File.ReadAllText("./puzzle.input")
|> parse
|> components
|> List.sum
|> printfn "The sum of all components is %A"