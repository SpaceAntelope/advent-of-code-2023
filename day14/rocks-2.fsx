#load "./rocks-common.fsx"

open System
open System.IO
open System.Collections.Generic
open ``Rocks-common``

let history = ResizeArray<int * int * string>()

let startOfRepetition =
    let discovered = HashSet<string>()

    "./puzzle.input"
    |> File.ReadAllText
    |> parse
    |> cycleGeneration
    |> Seq.takeWhile (fun (i, tab, scr) ->
        let h = str tab
        history.Add(i, scr, h)
        discovered.Add(h))
    |> Seq.last
    |> ignore

    history |> Seq.last
// |> fun (i, tab, scr) -> printfn "%d %d" i scr

let repetitionIndex, _, repetitionHash = startOfRepetition

let startOfPatternIndex =
    history
    |> Seq.find (fun (i, scr, h) -> h = repetitionHash)
    |> fun (idx, _, _) -> idx

let indexOfOneBillion =
    let patternLength = repetitionIndex - startOfPatternIndex
    (1000000000-1 - repetitionIndex) % patternLength

history
|> Seq.find (fun (i, scr, h) ->
    i = indexOfOneBillion + startOfPatternIndex)
|> fun (i, scr, h) -> printfn "Score at 1bn should be %d" scr

