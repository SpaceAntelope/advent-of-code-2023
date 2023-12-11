#load "./camel-cards-common.fsx"

open System
open System.Text.RegularExpressions
open ``Camel-cards-common``

let testdata = @"
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483"

// Test rankings
testdata 
|> Hand.Parse 
|> Array.map rankHandByType 
|> Array.map (fun x -> rankedHandIndex[x-1])        
|> printfn "%A"

testdata 
|> Hand.Parse 
|> Array.map rankHandByType 
|> Array.map (fun x -> rankedHandIndex[x-1])        
|> printfn "%A"

        
suite.ToCharArray() |> Array.map rankCard |> printfn "%A"


// Test bid calculation
let expected = 6440L

testdata
|> Hand.Parse
|> Array.sortWith handsComparer
|> Array.mapi (fun i hand -> hand.Bet * (int64 i+1L))
|> Array.sum
|> function
| x when x = expected  -> printfn "Calculate winnings passed."
| actual -> printfn $"Expected {expected} but got {actual}"

// Joker in play
open ``Camel-cards-common``.Joker
let expected2 = 5905L
testdata
|> Hand.Parse
|> Array.map applyJoker
|> Array.sortWith handsComparer
|> Array.mapi (fun i hand -> hand.Bet * (int64 i+1L))
|> Array.sum
|> function
| x when x = expected2  -> printfn "Calculate winnings with jokers in play passed."
| actual -> printfn $"Expected {expected2} but got {actual}"