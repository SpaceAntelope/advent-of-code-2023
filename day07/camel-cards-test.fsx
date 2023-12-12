#load "./camel-cards-common.fsx"
open System
open System.IO
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

        
NoJoker.suite.ToCharArray() |> Array.map NoJoker.rankCard |> printfn "%A"
Joker.suite.ToCharArray() |> Array.map Joker.rankCard |> printfn "%A"


// Test bid calculation
let expected = 6440L

testdata
|> Hand.Parse
|> Array.sortWith NoJoker.handsComparer
|> Array.mapi (fun i hand -> hand.Bet * (int64 i+1L))
|> Array.sum
|> function
| x when x = expected  -> printfn "Calculate winnings passed."
| actual -> failwith $"Expected {expected} but got {actual}"

// Joker in play

let expected2 = 5905L
testdata
|> Hand.Parse
|> Array.sortWith Joker.handsComparer
|> Array.mapi (fun i hand -> 
    printfn "%d %A %A" i hand.Hand (Joker.applyJoker hand).Hand
    hand.Bet * (int64 i+1L))
|> Array.sum
|> function
| x when x = expected2  -> printfn "Calculate winnings with jokers in play passed."
| actual -> failwith $"Expected {expected2} but got {actual}"