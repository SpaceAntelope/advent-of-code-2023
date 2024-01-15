#load "./lagoon-common.fsx"

open System
open System.IO
open ``Lagoon-common``


"./puzzle.input"
|> File.ReadAllText
|> parse
|> path
|> rebasePath
|> trenchArea
|> fun table -> File.WriteAllText("table.html", html table); table
|> fillTrench
|> fun table -> File.AppendAllText("table.html", html table); table
|> fun table -> table |> Seq.cast<char> |> Seq.countBy id
|> printfn "Trech holds %A m^3 of lava"
