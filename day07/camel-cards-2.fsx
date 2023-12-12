#load "./camel-cards-common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open ``Camel-cards-common``

let negate num = -1 * num

"./puzzle.input"
|> File.ReadAllText 
|> Hand.Parse
|> Array.sortWith Joker.handsComparer
|> Array.mapi (fun i hand -> hand, hand.Bet * (int64 i+1L))
|> Array.sumBy snd
|> printfn "Weighted bids sum when joker in play is %A"

