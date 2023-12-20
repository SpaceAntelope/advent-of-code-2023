#load "./springs-common.fsx"


open System
open System.IO
open System.Text
open System.Collections.Generic
open ``Springs-common``

"./puzzle.input"
|> File.ReadAllText
|> parse
|> Array.map (fun spring -> 
    permutations spring.Mask spring.CRC)
|> Array.sumBy Seq.length
|> printfn "Total count of possible arrangements is %d"