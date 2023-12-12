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