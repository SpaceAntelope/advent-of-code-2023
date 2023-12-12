open System
open System.IO
open System.Text.RegularExpressions

let parse (data: string) = 
    let instructions = Regex.Match(data.Trim(), "^[RL]+").Value
    let map = 
        data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) 
        |> Array.skip 1
        |> Array.map(fun line -> 
            let patterns = 
                Regex.Matches(line, "\\w{3}") 
                |> Seq.map _.Value 
                |> Array.ofSeq
            patterns.[0],(patterns.[1],patterns.[2]))
        |> Map.ofArray
    instructions.ToCharArray(), map


let rec search (steps: int) (currentKey: string) (instructions: char[]) (map: Map<string,string*string>) =    
    let instructionIndex = steps % instructions.Length
    let (L,R) = map.[currentKey]
    
    match instructions.[instructionIndex] with 
    | 'L' when L = "ZZZ" -> steps + 1
    | 'R' when R = "ZZZ" -> steps + 1
    | 'L' -> search (steps+1) L instructions map
    | 'R' -> search (steps+1) R instructions map
    | x -> failwith $"Unknown instruction {x}"