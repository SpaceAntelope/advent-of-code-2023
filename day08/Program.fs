open System.IO
open System.Text.RegularExpressions
open System
open System.Collections.Generic

type Instruction = L | R

let parse path =
    let lines = File.ReadAllLines(path)
   
    let instructions = 
        lines.[0].Trim().ToCharArray() 
        |> Array.map (function 'L' -> L | _ -> R)

    let map =
        lines
        |> Array.skip 2
        |> Array.map (fun line ->
            let patterns = 
                Regex.Matches(line, "\\w{3}") 
                |> Seq.map _.Value 
                |> Array.ofSeq
            patterns.[0], (patterns.[1], patterns.[2]))
        |> readOnlyDict

    instructions, map

let followInstructions start (instructions: Instruction[]) (map: IReadOnlyDictionary<string,(string * string)>) = 
    let mutable index = 0
    let mutable label = start// = map.[start]
    seq {
        while true do
            let i = instructions.[index%instructions.Length]
            yield label
            label <-
               match i with
                | L -> fst map.[label]
                | R -> snd map.[label]                    
    }

let instructions, map = 
    parse "./input/puzzle.example2"

let seqs = 
    map.Keys 
    |> Seq.filter (fun key -> key.EndsWith('A'))
    |> Seq.map (fun start -> followInstructions start instructions map)
    |> Array.ofSeq

let result = Array.create seqs.Length ""
let mutable steps = 0
while result |> Array.forall (fun x -> not <| x.EndsWith('Z')) do
    printfn "%A" result
    for i in 0..seqs.Length-1 do
        result.[i] <- seqs.[i] |> Seq.take 1 |> Seq.head
    steps <- steps + 1

printfn "%A" map
printfn "It takes %d steps before you're only on nodes that end with Z" steps
