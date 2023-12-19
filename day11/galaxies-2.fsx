#load "./galaxies-common.fsx"


open System
open System.IO
open System.Text
open System.Collections.Generic
open ``Galaxies-common``


let universe = 
    "./puzzle.input"
    |> File.ReadAllText
    |> parse

let points = 
    universe
    |> numberedPoints

let emptyCols = emptyColumns universe
let emptyLines = emptyRows universe

points 
|> allDistancePairs
|> Seq.sumBy (fun (p1,p2) -> int64 <| expandedManhattanDistance points[p1] points[p2] emptyLines emptyCols 1000000)
|> printfn "Total distance in older universe seems to be %d"