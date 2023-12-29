#load "./rocks-common.fsx"

open System
open System.IO
open System.Collections.Generic
open ``Rocks-common``

(*
    Every arrangement deterministaclly cycles into a next arrangement, so as soon as an arrangement
    reappears the pattern should repeat. Finding the 1 billionth arrangement is a matter of finding 
    the repeating pattern and then mapping its index to the 1e9th index.
*)

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
    |> ignore // we ignore the item returned because takewhile stops the index before the repeating pattern is discovered

    history |> Seq.last

let repetitionIndex, _, repetitionHash = startOfRepetition

let startOfPatternIndex =
    history
    |> Seq.find (fun (i, scr, h) -> h = repetitionHash)
    |> fun (idx, _, _) -> idx


let indexOfOneBillion =
    // every number is a zero based index so we use 1e9-1 istead of 1e9 so we don't need further conversions down the line
    let patternLength = repetitionIndex - startOfPatternIndex
    (1000000000-1 - repetitionIndex) % patternLength

history
|> Seq.find (fun (i, scr, h) ->
    i = indexOfOneBillion + startOfPatternIndex)
|> fun (i, scr, h) -> printfn "Score at 1bn should be %d" scr