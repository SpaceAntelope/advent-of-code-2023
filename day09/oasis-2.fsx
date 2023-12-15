#load "./oasis-common.fsx"

open System
open ``Oasis-common``
open System.IO


File.ReadAllText("./puzzle.input")
|> parse
|> Array.map (fun line -> diff line |> Array.append [|line|])
|> Array.map sumExtrapolatedPrevious
|> Array.sum
|> printfn "Sum of exptrapolated preceeding elements in dataset is %A"




