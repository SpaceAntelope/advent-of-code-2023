#load "./springs-common.fsx"
#load "../global.fsx"


open System
open System.IO
open System.Text
open System.Collections.Generic
open ``Springs-common``
open System.Diagnostics
open System.Threading.Tasks
open System.Text.RegularExpressions

// let parse path =
//     path
//     |> File.ReadAllLines
// type Spring = {
//     Mask : string
//     CRC: int[]
// }

let unfold (spring: Spring) =
    { Mask = Array.create 5 spring.Mask |> Array.reduce (sprintf "%s?%s")
      CRC = Array.replicate 5 spring.CRC |> Array.collect id }

"./puzzle.input"
|> File.ReadAllText
|> parse
|> Array.map unfold
// |> Array.sortBy (fun x -> Regex.Matches(x.Mask, "\?").Count)
|> Array.chunkBySize 50
|> Array.Parallel.mapi (fun chunkIndex springs -> 
        springs 
        |> Array.mapi (fun i s -> i,s)
        |> Array.sumBy (fun (i,spring) -> 
            let sw = Stopwatch()
            sw.Start()
            // printfn $"Case %d{i} %A{spring.Mask} started"
            let count = permutationsCount spring.Mask spring.CRC
            sw.Stop() 
            printfn $"Case %d{chunkIndex * 50 + i} %A{spring.Mask} finished, found {count} arrangements in {sw.Elapsed} at %0.3f{float count/sw.Elapsed.TotalSeconds} per sec"
            count)        
    )
|> Array.sum
|> printfn "Total count of possible arrangements is %d"