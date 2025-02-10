// open System
open System.IO

type SpringStatus = 
    | Op | Dmg | Unk 
    with 
        static member From(c: char) =
            match c with
            | '.' -> Op
            | '#' -> Dmg
            | '?' -> Unk
            | x -> failwith $"What even is %c{x}"
        member x.AsChar = 
            match x with
            | Op -> '.'
            | Unk -> '?'
            | Dmg -> '#'
        member x.AsGlyph = 
            match x with
            | Op ->  "⛽"
            | Unk -> "❓"
            | Dmg -> "⛓️‍💥"

let c2str (chars: char[]) = chars |> Array.fold (sprintf "%s%c") ""

type ConditionRecord = {
    Status: SpringStatus[]
    CRC: int[]
} with 
    member x.Unfold() = {
        Status = 
            Array.create 5 x.Status 
            |> Array.reduce (fun s c -> Array.concat [|s;[|Unk|];c|])
        CRC = Array.create 5 x.CRC |> Array.concat  }
    member x.AsGlyph() = 
        {| x with Status = x.Status |> Array.map _.AsGlyph |}
    member x.AsChar() = 
        {| x with Status = x.Status |> Array.map _.AsChar |}
    member x.ToStringChar() = x.AsChar() |> (fun c -> sprintf "%s %s" (c.Status |> c2str) (c.CRC |> Array.map string |> String.concat ","))
    member x.ToStringGlyph() = x.AsGlyph() |> (fun c -> sprintf "%s %s" (c.Status |> String.concat "") (c.CRC |> Array.map string |> String.concat ","))


let parse path =
    
    path
    |> File.ReadAllLines 
    |> Array.map _.Split(' ')
    |> Array.map (fun arr -> {
            Status = arr.[0].ToCharArray() |> Array.map SpringStatus.From
            CRC = arr.[1].Split(',') |> Array.map (string>>int)    })
    |> Array.map (fun record ->  record.Unfold())  


let calculateAllCombinations (cr: ConditionRecord) = 
    let length = cr.Status.Length
    let minLengthForCRC= cr.CRC.Length - 1 + (cr.CRC |> Array.sum)
    printfn "Status length: %d CRC Length: %d" length minLengthForCRC

"./input/puzzle.example"
|> parse 
|> Array.iter (fun cr -> 
        printfn "%s" <| cr.ToStringChar()
        cr |> calculateAllCombinations)
