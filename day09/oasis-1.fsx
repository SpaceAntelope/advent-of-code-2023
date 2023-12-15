#load "./oasis-common.fsx"

open System
open ``Oasis-common``
open System.IO


File.ReadAllText("./puzzle.input")
|> parse
|> Array.map (fun line -> diff line |> Array.append [|line|])
|> Array.map sumExtrapolatedNext
|> Array.sum
|> printfn "Sum of exptrapolated following elements in dataset is %A"




