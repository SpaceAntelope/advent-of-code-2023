#load "./camel-cards-common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open ``Camel-cards-common``


"./puzzle.input"
|> File.ReadAllText 
|> Hand.Parse
|> Array.sortWith handsComparer
|> Array.mapi (fun i hand -> hand.Bet * (int64 i+1L))
|> Array.sum
|> printfn "Weighted bids sum is %A"