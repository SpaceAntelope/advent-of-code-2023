#load "./galaxies-common.fsx"


open System
open System.IO
open System.Text
open System.Collections.Generic
open ``Galaxies-common``

let points = 
    "./puzzle.input"
    |> File.ReadAllText
    |> parse
    |> expand
    |> numberedPoints

points 
|> allDistancePairs
|> Seq.sumBy (fun (p1,p2) -> manhattanDistance points[p1] points[p2])
|> printfn "Total distance seems to be %d"