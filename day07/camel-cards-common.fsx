open System
open System.Text.RegularExpressions

type Hand = {
    Hand : string
    Bet: int64
} with static Parse (data: string) =
        data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map (fun str -> Regex.Split(str,"\\s+"))
        |> Array.map (fun arr -> {
            Hand = arr.[0]
            Bet = Convert.ToInt64 arr.[1]
        })

let suite = "AKQJT98765432" 

let rankedHandIndex = [|
    "Highest"
    "TwoOfAKind"
    "TwoPairs"
    "ThreeOfAKind"
    "FullHouse"
    "FourOfAKind"
    "FiveOfAKind"
|]

let rankHandByType (hand: Hand) =
    hand.Hand.ToCharArray() 
    |> List.ofArray
    |> List.countBy id
    |> List.map snd
    |> List.sortDescending
    |> function
    | count::_ when count = 5  -> 7
    | count::_ when count = 4  -> 6
    | x::y::_ when x = 3 && y = 2 -> 5
    | count::_ when count = 3  -> 4
    | x::y::_ when x = 2 && y = 2 -> 3
    | count::_ when count = 2  -> 2
    | _ -> 1

let rankCard (card : char)=
    suite.Length - suite.IndexOf(card)

let handsComparer (x : Hand) (y: Hand) = 
    let typeX = rankHandByType x
    let typeY = rankHandByType y

    match typeX.CompareTo(typeY) with
    | 0 ->     
        Array.zip (x.Hand.ToCharArray()) (y.Hand.ToCharArray())
        |> Array.map (fun (c1,c2) -> (rankCard c1), (rankCard c2))
        |> Array.tryFind (fun (r1,r2) -> r1 <> r2)
        |> function 
        | Some (r1,r2) -> r1.CompareTo(r2)
        | None -> 0
    | result -> result
 
module Joker =
    let suite = "AKQT98765432J"

    let rankCard (card : char) = suite.Length - suite.IndexOf(card)

    let applyJoker (hand: Hand) = 
        if hand.Hand.Contains('J')
        then 
            let mostFrequentCard = 
                hand.Hand.ToCharArray() 
                |> List.ofArray
                |> List.countBy id
                |> List.maxBy snd
                |> fst
            { hand with Hand = hand.Hand.Replace('J', mostFrequentCard) }
        else hand

    let handsComparer (x : Hand) (y: Hand) = 
        let typeX = rankHandByType x
        let typeY = rankHandByType y

        match typeX.CompareTo(typeY) with
        | 0 ->     
            Array.zip (x.Hand.ToCharArray()) (y.Hand.ToCharArray())
            |> Array.map (fun (c1,c2) -> (rankCard c1), (rankCard c2))
            |> Array.tryFind (fun (r1,r2) -> r1 <> r2)
            |> function 
            | Some (r1,r2) -> r1.CompareTo(r2)
            | None -> 0
        | result -> result