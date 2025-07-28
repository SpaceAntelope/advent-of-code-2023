namespace Day20.Tests

module FilpFlopTests = 

    open Xunit
    open Day20.Model
    open Common

    let generateNetwork() = 
        let button = Button()
        let broadcaster = Broadcaster([|"a"|])
        let ff = FlipFlop("a", [|"x";"y";"z"|])
        let x = Neutral("x")
        let y = Neutral("y")
        let z = Neutral("z")

        let network : ModuleBase array = [|button;broadcaster;ff;x;y;z|]
        let index = network |> Seq.map(fun x -> x.Name, x) |> readOnlyDict

        index

    [<Fact>]
    let ``Default flip state is off``() = 
        let ff = FlipFlop("ff",[||])
        
        Assert.Equal(ff.State , Off)

    let TruthTableData = 
        let data = TheoryData<FlipState, Pulse, FlipState>()
        data.Add(Off, Low, On)
        data.Add(Off, High, Off)
        data.Add(On, Low, Off)
        data.Add(On, High, On)
        data

    [<Theory>]
    [<MemberData(nameof(TruthTableData))>]
    let ``flip flop with on state should switch off when low pulsed`` (initialState: FlipState) (input: Pulse) (expectedState: FlipState) =
        
        let ff = FlipFlop("ff",[||])
        ff.State <- initialState

        // Arrange
        match input with 
        | High -> ff.SendHighPulseFrom("x")
        | Low -> ff.SendLowPulseFrom("x")

        let actual = ff.State

        Assert.Equal(expectedState,actual)

    [<Theory>]
    [<InlineData(3, "On")>]
    [<InlineData(7, "On")>]
    [<InlineData(21, "On")>]
    [<InlineData(2, "Off")>]
    [<InlineData(8, "Off")>]
    [<InlineData(22, "Off")>]
    let ``flip flop should return to initial state after even low pulses or change after odd low pulses``(pulseCount: int) (state: string) = 
        let ff = FlipFlop("ff",[||])
        let expectedState = stateParse state
        for _ in 1..pulseCount do
            ff.SendLowPulseFrom("x")

        Assert.Equal(expectedState, ff.State)