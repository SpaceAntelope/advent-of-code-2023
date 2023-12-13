#load "./maps-common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open ``Maps-common``

let testData1 = @"
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"


let expected1 = 2

parse testData1
||> search 0 "AAA"
|> function 
| steps when steps = expected1 -> printfn "Passed 2 step search"
| actual -> failwith $"Expected {expected1} steps but got {actual}"


let testData2 = @"
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)

"

let expected2 = 6

parse testData2
||> search 0 "AAA"
|> function 
| steps when steps = expected2 -> printfn "Passed 6 step search"
| actual -> failwith $"Expected {expected2} steps but got {actual}"

let testData3= @"
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

let expected3 = 6
let instr, map2 = 
    parse testData3
let startingKeys = 
    map2.Keys 
    |> Seq.filter _.EndsWith('A') 
    |> Array.ofSeq

// printfn "%A" map2
search2 0 startingKeys instr map2
|> function 
| steps when steps = expected3 -> printfn "Passed parallel search test"
| actual -> failwith $"Expected {expected3} steps but got {actual}"