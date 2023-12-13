#load "./maps-common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open ``Maps-common``

let instructions, map = 
    "./puzzle.input"
    |> File.ReadAllText 
    |> parse 

let startingKeys = 
    map.Keys 
    |> Seq.filter _.EndsWith('A') 
    |> Array.ofSeq

printfn $"Starting keys count: {startingKeys.Length}"
printfn $"Instructions length: {instructions.Length}"

search2 0 startingKeys instructions map
|> printfn "Parallel map challenge completed in %d steps"