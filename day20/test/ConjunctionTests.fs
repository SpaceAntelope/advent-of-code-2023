namespace Day20.Tests

module ConjunctionTests =

    open Xunit
    open Day20.Model
    open System.Text
    open Common
    open System.Collections.Generic




    [<Theory>]
    [<InlineData("High", "Low")>]
    [<InlineData("Low", "High")>]
    let ``Default behavior (truth table)`` (incoming: string) (output: string) =
        let incomingPulse = pulseParse incoming
        let expectedOutput = pulseParse output
        let con = Conjunction("con", [||])

        match incomingPulse with
        | High -> con.SendHighPulseFrom("x")
        | Low -> con.SendLowPulseFrom("x")

        printfn "%A" con.State
        let actual = con.Output()
        Assert.Equal(expectedOutput, actual)


    let MultiInputData =
        let dic (arr: (string * Pulse) seq) =
            Dictionary<string, Pulse>(
                arr
                |> Seq.map (fun (x, y) -> KeyValuePair<string, Pulse>(x, y))
            )

        let result =
            TheoryData<Dictionary<string, Pulse>, Dictionary<string, Pulse>, Pulse>()

        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", High; "y", High; "z", High ], Low)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", High; "y", High; "z", Low ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", High; "y", Low; "z", Low ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", Low; "y", Low; "z", Low ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", Low; "y", High; "z", Low ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", Low ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", Low; "y", Low ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [ "x", Low; "z", High ], High)
        result.Add(dic [ "x", Low; "y", Low; "z", Low ], dic [  ], High)
        result.Add(dic [ "x", High; "y", High; "z", High ], dic [ ], Low)
        result.Add(dic [ "x", High; "y", Low; "z", High ], dic ["y",High ], Low)
        result


    [<Theory>]
    [<MemberData(nameof (MultiInputData))>]
    let ``Truth table for multiple inputs``
        (state: Dictionary<string, Pulse>)
        (incoming: Dictionary<string, Pulse>)
        (expectedOutput: Pulse)
        =

        let con = Conjunction("con", [||], state)

        for incoming in incoming do
            match incoming.Value with
            | High -> con.SendHighPulseFrom(incoming.Key)
            | Low -> con.SendLowPulseFrom(incoming.Key)

        printfn "%A" con.State
        let actual = con.Output()
        Assert.Equal(expectedOutput, actual)
