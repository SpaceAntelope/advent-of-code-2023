namespace Day20.Tests

module ParseTests =

    open Xunit
    open Day20.Model
    open System
    open System.Text

    let dontLog = fun (msg:string) -> ()
    let flatten arr = arr |> Seq.collect id

    let getConjunctionIndex (source: ModuleBase seq) =
        source
        |> Seq.filter (fun x -> x :? Conjunction) 
        |> Seq.cast<Conjunction>
        |> Seq.map (fun x -> x.Name, x)
        |> readOnlyDict

    [<Fact>]
    let ParseExample1() =

        let expected : ModuleBase[]= [|
            Button()
            Broadcaster([|"a";"b";"c"|])
            FlipFlop("a", [|"b"|])
            FlipFlop("b", [|"c"|])
            FlipFlop("c", [|"inv"|])
            Conjunction("inv", [|"a"|]) |]

        let actual, _ = 
            Day20.Part1.parse "./input/puzzle.example-1" 

        Assert.Equivalent(expected, actual)

        let conjunctions = actual |> getConjunctionIndex

        Assert.Equivalent(["c"], conjunctions.["inv"].State.Keys)

    [<Fact>]
    let ParseExample2() =

        let expected : ModuleBase[] = [|
            Button()
            Broadcaster([|"a"|])
            FlipFlop("a", [|"inv";"con"|])
            Conjunction("inv", [|"b"|])
            FlipFlop("b", [|"con"|])
            Conjunction("con", [|"output"|]) 
            Neutral("output") |]

        let actual, _ = 
            Day20.Part1.parse "./input/puzzle.example-2" 

        Assert.Equivalent(expected, actual)

        let conjunctions = actual |> getConjunctionIndex

        Assert.Equivalent(["a"], conjunctions.["inv"].State.Keys)
        Assert.Equivalent(["a"; "b"], conjunctions.["con"].State.Keys)


    [<Fact>]
    let processNetworkOnce() =
        let expectedLo = 8
        let expectedHi = 4
        let expectedLog =
            "button -low-> broadcaster
broadcaster -low-> a
broadcaster -low-> b
broadcaster -low-> c
a -high-> b
b -high-> c
c -high-> inv
inv -low-> a
a -low-> b
b -low-> c
c -low-> inv
inv -high-> a
"
            |> fun x -> x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) 
            |> Array.filter (fun x -> (x.Contains "Step") |> not) 
            |> Array.map PulsingEdge.Parse

        let network, _ = Day20.Part1.parse "./input/puzzle.example-1"
        
        let edges = Day20.Part1.processPulses' network |> flatten

        let actual = Day20.Part1.evaluate network

        // printfn "Actual\n%O" actualLog
        
        Assert.Equivalent(expectedLog, edges)
        Assert.Equal(expectedHi, actual.Hi)
        Assert.Equal(expectedLo, actual.Lo)

    [<Fact>]
    let processNetworkFourTimes() =
        let expectedEdges = 
            [|
            "button -low-> broadcaster
broadcaster -low-> a
a -high-> inv
a -high-> con
inv -low-> b
con -high-> output
b -high-> con
con -low-> output"
            "button -low-> broadcaster
broadcaster -low-> a
a -low-> inv
a -low-> con
inv -high-> b
con -high-> output"
            "button -low-> broadcaster
broadcaster -low-> a
a -high-> inv
a -high-> con
inv -low-> b
con -low-> output
b -low-> con
con -high-> output"
            "button -low-> broadcaster
broadcaster -low-> a
a -low-> inv
a -low-> con
inv -high-> b
con -high-> output" |] 
            |> Array.map (fun step ->
                    step.Split(Environment.NewLine)
                    |> Array.map PulsingEdge.Parse)

        let network, modules = Day20.Part1.parse "./input/puzzle.example-2"
        let actualEdges0 = Day20.Part1.processPulses' network |> flatten
        Assert.Equivalent(expectedEdges.[0], actualEdges0)
        
        let actualEdges1 = Day20.Part1.processPulses' network|> flatten
        Assert.Equivalent(expectedEdges.[1], actualEdges1)
        
        let actualEdges2 = Day20.Part1.processPulses' network|> flatten
        Assert.Equivalent(expectedEdges.[2], actualEdges2)

        let actualEdges3 = Day20.Part1.processPulses' network|> flatten
        Assert.Equivalent(expectedEdges.[3], actualEdges3)

        let actualEdges4 = Day20.Part1.processPulses' network|> flatten
        Assert.Equivalent(actualEdges0, actualEdges4)

    [<Theory>]
    [<InlineData("./input/puzzle.example-1", 32000000)>]
    [<InlineData("./input/puzzle.example-2", 11687500)>]
    let processNetwork1000Times(path: string, expectedEvaluation: int) =
        let network, _ = Day20.Part1.parse path
        
        for _ in 1..1000 do Day20.Part1.processPulses dontLog network

        let actual = Day20.Part1.evaluate network

        // printfn "%s %A" path actual

        Assert.Equal(expectedEvaluation, actual.Evaluation)

