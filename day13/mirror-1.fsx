#load "./mirror-common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open ``Mirror-common``

"./puzzle.input"
|> File.ReadAllText
|> fun text -> Regex.Split(text, "^\\s*$", RegexOptions.Multiline)
|> Array.map parse
|> Array.sumBy symScore
|> printfn "Reflection hash is %d"