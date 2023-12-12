#load "./maps-common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open ``Maps-common``

"./puzzle.input"
|> File.ReadAllText 
|> parse 
||> search 0 "AAA"
|> printfn "First map challenge completed in %d steps"