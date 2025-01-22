open System.IO
open System.Text.RegularExpressions
open System
open System.Collections.Generic

type Instruction = L | R

type StatefulSeq(start:string, instructions: Instruction[], map: IReadOnlyDictionary<string,(string * string)>) = 
    let mutable index = 0
    let mutable label = start

    let mutable _lastState  = (0,"",L)
    member x.take(n: int) = [ for i in 0..n-1 -> x.next() ]
    member x.instructionsLength = instructions.Length
    member x.lastState() = _lastState
    member x.state() = index, label, instructions.[index%x.instructionsLength] 
    member x.currentInstruction() = instructions.[index%x.instructionsLength]
    member x.currentIndex() = index
    member x.currentLabel() = label
    member x.next() =
        _lastState <- (index, label, instructions.[index%instructions.Length])
        // printfn "Current state: label: %s index: %d" label index
        label <-
               match instructions.[index%instructions.Length] with
                | L -> fst map.[label]
                | R -> snd map.[label]            
        index <- index + 1
        label

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

let findStepsByBruteForce path = 
    let instructions, map = 
        parse path

    let seqs = 
        map.Keys 
        |> Seq.filter (fun key -> key.EndsWith('A'))
        // |> Seq.map (fun start -> followInstructions start instructions map)
        |> Seq.map (fun start -> StatefulSeq(start, instructions, map))
        |> Array.ofSeq

    let combinedStates = Array.create seqs.Length ""
    let mutable steps = 0
    while combinedStates |> Array.forall (fun x -> x.EndsWith('Z')) |> not do
        for i in 0..seqs.Length-1 do
            combinedStates.[i] <- seqs.[i].next() 
                // |> Seq.last
        // printfn "%04d: %A" steps combinedStates
        if steps % 5000000 = 0 then printfn "Currently at step %dM" (steps/1000000)
        steps <- steps + 1

    printfn "Result: %A" combinedStates

    steps


"./input/puzzle.example2"
|> findStepsByBruteForce
|> Common.Assertions.shouldBe 6

(* Well this didn't work *)
// "./input/puzzle.input"
// |> findStepsByBruteForce
// |> printfn "It takes %d steps before you're only on nodes that end with Z"

let checkForPathRecyclingByLookingForStartNodesWithSameIndexDownThePath() = 
    printfn "Do start nodes recycle?" 
    let instructions, map = 
        parse "./input/puzzle.input"

    let seqs = 
        map.Keys 
        |> Seq.filter (fun key -> key.EndsWith('A'))
        |> Seq.map (fun start -> StatefulSeq(start, instructions, map))
        |> Array.ofSeq

    seqs |> Array.map _.currentLabel() |> printfn "%A"    

    for s in seqs do

        let targetState = 
            s.next() |> ignore
            s.state()

        let startLabel =  s.next()
        printfn "Target %A" targetState
        let (targetIndex,targetLabel,_) = targetState

        let mutable label = startLabel
        while not (s.currentLabel() = targetLabel && (s.currentIndex() % s.instructionsLength) = targetIndex) && s.currentIndex() < 1000000 do 
            label <- s.next()
            if s.currentIndex() % 1000000 = 0 then printfn "%A" <| s.state()

        printfn "Final state: %A\n" <| s.state()

let checkForPathRecyclingByLookingForEndNodesWithSameIndexDownThePath() = 
    printfn "Do end nodes recycle?" 
    let instructions, map = 
        parse "./input/puzzle.input"

    let seqs = 
        map.Keys 
        |> Seq.filter (fun key -> key.EndsWith('A'))
        |> Seq.map (fun start -> 
            let ssq = StatefulSeq(start, instructions, map)
            while ssq.currentLabel().EndsWith 'Z' |> not do ssq.next() |> ignore
            ssq)
        |> Array.ofSeq

    for s in seqs do

        let targetState = s.state()

        let startLabel =  s.next()
        printfn "Target %A" targetState
        let (targetIndex,targetLabel,_) = targetState

        let mutable label = startLabel
        while not (s.currentLabel() = targetLabel && (s.currentIndex() % s.instructionsLength) = targetIndex % s.instructionsLength) && s.currentIndex() < 1000000 do 
            
            label <- s.next()
            if s.currentIndex() % 1000000 = 0 then printfn "%A" <| s.state()

        printfn "Final state: %A\n" <| s.state()

checkForPathRecyclingByLookingForStartNodesWithSameIndexDownThePath() (* Nope *)

checkForPathRecyclingByLookingForEndNodesWithSameIndexDownThePath() (* They sure do! *)

let calculateStepsByFindingLCM(path) = 
    let instructions, map = 
        parse path

    // greatest common denominator & least common multiplier algos thanks to wiki
    let rec gcd_euclidean  x y = 
        let remainder = x % y
        if remainder = 0L then y
        else gcd_euclidean y remainder
            
    let lcm x y = x * y / gcd_euclidean x y

    map.Keys 
    |> Seq.filter (fun key -> key.EndsWith('A'))
    |> Seq.map (fun start -> 
        let ssq = StatefulSeq(start, instructions, map)
        while ssq.currentLabel().EndsWith 'Z' |> not do ssq.next() |> ignore
        int64 <| ssq.currentIndex())
    |> Seq.sort 
    |> Seq.reduce (fun state length -> lcm state length)

"./input/puzzle.example2"
|> calculateStepsByFindingLCM
|> Common.Assertions.shouldBe 6

"./input/puzzle.input"
|> calculateStepsByFindingLCM
|> printfn "It would take %d steps before you're only on nodes that end with Z"