open System
open System.IO
open System.Text.RegularExpressions

let parse (data: string) =
    let instructions = Regex.Match(data.Trim(), "^[RL]+").Value

    let map =
        data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.skip 1
        |> Array.map (fun line ->
            let patterns = Regex.Matches(line, "\\w{3}") |> Seq.map _.Value |> Array.ofSeq
            patterns.[0], (patterns.[1], patterns.[2]))
        |> Map.ofArray

    instructions.ToCharArray(), map

[<TailCall>]
let rec search (steps: int) (currentKey: string) (instructions: char[]) (map: Map<string, string * string>) =
    let instructionIndex = steps % instructions.Length
    let (L, R) = map.[currentKey]

    match instructions.[instructionIndex] with
    | 'L' when L = "ZZZ" -> steps + 1
    | 'R' when R = "ZZZ" -> steps + 1
    | 'L' -> search (steps + 1) L instructions map
    | 'R' -> search (steps + 1) R instructions map
    | x -> failwith $"Unknown instruction {x}"



let search2 (steps: int) (currentKeys: string[]) (instructions: char[]) (map: Map<string, string * string>) =
    
    // [<TailCall>]
    let rec internalSearch (steps: int64) (keys: string array) = 
        if steps % 1000000L = 0 then printfn $"Currently on step {steps}"
        
        let instructionIndex  = int32 (steps % instructions.LongLength)
        let instruction = instructions.[instructionIndex]

        let nextKeys =
            keys 
            |> Array.map (fun key -> map.[key])
            |> Array.map (fun (L, R) -> if instruction = 'R' then R else L)

        // printfn "%d %A - %c -> %A" steps keys instruction nextKeys

        if (nextKeys |> Array.forall _.EndsWith('Z')) then
            steps + 1L
        else
            internalSearch (steps + 1L) nextKeys

    internalSearch 0L currentKeys


// let search3 (instructions: char[]) (map: Map<string, string * string>) =
    
//     let rec internalSearch (steps: int) (currentKey: string)  =
//         let instructionIndex = steps % instructions.Length
//         let instruction = instructions.[instructionIndex]
        
//         let (L, R) = map.[currentKey]

//         match instruction with
//         | 'L' when L.EndsWith('Z') -> steps + 1
//         | 'R' when R.EndsWith('Z') -> steps + 1
//         | 'L' -> internalSearch (steps + 1) L 
//         | 'R' -> internalSearch (steps + 1) R 
//         | x -> failwith $"Unknown instruction {x}"
