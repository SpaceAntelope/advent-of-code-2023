#load "./lava-common.fsx"

open ``Lava-common``

open System
open System.IO

let table =
    "./puzzle.input" |> File.ReadAllText |> parse

table
|> startingVectors
|> Array.map (fun (dir, (row, col)) ->
    async {
        return
            table
            |> tripTheLight dir (row, col)
            |> countEnergized
    })
|> Async.Parallel
|> Async.RunSynchronously
|> Array.max
|> printfn "Max %d energized tiles."
