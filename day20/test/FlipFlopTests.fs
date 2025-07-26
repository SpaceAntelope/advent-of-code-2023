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



    [<Theory>]
    [<InlineData("Off","High","Off")>]
    [<InlineData("Off","Low", "On")>]
    [<InlineData("On","High","On")>]
    [<InlineData("On","Low", "Off")>]
    let ``flip flop with on state should switch off when low pulsed`` (initialState: string) (input: string) (state: string) =
        let inputPulse = pulseParse input
        let initState = stateParse initialState
        let expected = stateParse state

        let ff = FlipFlop("ff",[||])
        ff.State <- initState

        // Arrange
        match inputPulse with 
        | High -> ff.SendHighPulseFrom("x")
        | Low -> ff.SendLowPulseFrom("x")

        let actual = ff.State

        Assert.Equal(expected,actual)

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