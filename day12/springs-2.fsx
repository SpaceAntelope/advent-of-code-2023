#load "./springs-common.fsx"


open System
open System.IO
open System.Text
open System.Collections.Generic
open ``Springs-common``

"./puzzle.input"
|> File.ReadAllText
|> parse
|> Array.map unfold
|> Array.mapi (fun i spring -> 
    printf $"Case %d{i} %A{spring.Mask}: "
    permutations spring.Mask spring.CRC
    |> Array.ofSeq    
    |> fun x -> printfn $"Count: {x.LongLength}"; x.LongLength)
|> Array.sum
|> printfn "Total count of possible arrangements is %d"