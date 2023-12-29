#load "./hash-common.fsx"

open ``Hash-common``

open System
open System.IO

"./puzzle.input"
|> File.ReadAllText
|> _.Split(',')
|> Array.map _.Trim()
|> Array.sumBy hash
|> printfn "Sum of the results is %A"