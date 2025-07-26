namespace Day20.Tests

module ParseTests =

    open Xunit
    open Day20.Model
    open System.Text

    let dontLog = fun (msg:string) -> ()

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
        let expectedLog ="
Step: 1
button --Low-> broadcaster

Step: 2
broadcaster --Low-> a
broadcaster --Low-> b
broadcaster --Low-> c

Step: 3
a --High-> b
b --High-> c
c --High-> inv

Step: 4
inv --Low-> a

Step: 5
a --Low-> b

Step: 6
b --Low-> c

Step: 7
c --Low-> inv

Step: 8
inv --High-> a
"

        let network, _ = Day20.Part1.parse "./input/puzzle.example-1"
        
        let actualLog = StringBuilder()
        
        Day20.Part1.processPulses (fun msg -> actualLog.AppendLine(msg) |> ignore) network

        let actual = Day20.Part1.evaluate network

        printfn "Actual\n%O" actualLog
        
        Assert.Equal(expectedLog, actualLog.ToString())
        Assert.Equal(expectedHi, actual.Hi)
        Assert.Equal(expectedLo, actual.Lo)

    [<Theory>]
    [<InlineData("./input/puzzle.example-1", 32000000)>]
    [<InlineData("./input/puzzle.example-2", 11687500)>]
    let processNetwork1000Times(path: string, expectedEvaluation: int) =
        let network, _ = Day20.Part1.parse path
        
        for _ in 1..1000 do Day20.Part1.processPulses dontLog network

        let actual = Day20.Part1.evaluate network

        Assert.Equal(expectedEvaluation, actual.Evaluation)

