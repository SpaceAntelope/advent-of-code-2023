open System
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
        // member x.AsGlyph = 
        //     match x with
        //     | Op -> '.'
        //     | Unk -> '?'
        //     | Dmg -> '💔'

type ConditionRecord = {
    Status: SpringStatus[]
    CRC: int[]
} with 
    member x.Unfold() = {
        Status = Array.create 5 x.Status |> Array.reduce (fun s c -> Array.concat [|s;[|Unk|];c|])
        CRC = Array.create 5 x.CRC |> Array.concat  }

let parse path =
    
    path
    |> File.ReadAllLines 
    |> Array.map _.Split(' ')
    |> Array.map (fun arr -> {
            Status = arr.[0].ToCharArray() |> Array.map SpringStatus.From
            CRC = arr.[1].Split(',') |> Array.map (string>>int)    })
    |> Array.map (fun record -> record, record.Unfold())

"./input/puzzle.example"
|> parse 
|> printfn "%A"