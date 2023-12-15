#load "./oasis-common.fsx"

open System
open ``Oasis-common``

let testData = @"
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"

let expectedSumsOfExtrapolated = [|18;28;68|]

parse testData
|> Array.map (fun line -> diff line |> Array.append [|line|])
|> Array.map (fun layer -> layer, sumExtrapolatedNext layer)
|> Array.map snd
|> function
| actual when actual = expectedSumsOfExtrapolated -> printfn "sum extrapolated following elements test passed"
| actual -> printfn $"Expected {expectedSumsOfExtrapolated} but got {actual}"

let expectedSumOfExtrapolatedPrevious = 2 

parse testData
|> Array.map (fun line -> diff line |> Array.append [|line|])
|> Array.map (fun layer -> layer, sumExtrapolatedPrevious layer)
|> Array.sumBy snd
|> function
| actual when actual = expectedSumOfExtrapolatedPrevious -> printfn "sum extrapolated preceeding elements test passed"
| actual -> printfn $"Expected {expectedSumOfExtrapolatedPrevious} but got {actual}"

