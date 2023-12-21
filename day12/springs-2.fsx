#load "./springs-common.fsx"


open System
open System.IO
open System.Text
open System.Collections.Generic
open ``Springs-common``
open System.Diagnostics
open System.Threading.Tasks
open System.Text.RegularExpressions





"./puzzle.input"
|> File.ReadAllText
|> parse
|> Array.sortBy (fun x -> Regex.Matches(x.Mask, "\?").Count)
|> Array.map unfold
|> Array.chunkBySize 50
|> Array.mapi (fun chunkIndex springs -> 
    async {
        return springs 
        |> Array.mapi (fun i s -> i,s)
        |> Array.sumBy (fun (i,spring) -> 
            let sw = Stopwatch()
            sw.Start()
            // printfn $"Case %d{i} %A{spring.Mask} started"
            let count = permutationsCount spring.Mask spring.CRC
            sw.Stop() 
            printfn $"Case %d{chunkIndex * 50 + i} %A{spring.Mask} finished, found {count} arrangements in {sw.Elapsed} at %0.3f{float count/sw.Elapsed.TotalSeconds} per sec"
            count)        
    })
|> Async.Parallel
|> Async.RunSynchronously 
|> Array.sum
|> printfn "Total count of possible arrangements is %d"