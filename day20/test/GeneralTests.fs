namespace Day20.Tests

module GeneralTests =

    open Xunit
    open Day20.Model
    open Common

    let PulsingEdgeData =
        let data = TheoryData<string, PulsingEdge>()
        
        data.Add("button -low-> broadcaster", PulsingEdge.Create "button" Low "broadcaster")
        data.Add("broadcaster -low-> a", PulsingEdge.Create "broadcaster" Low "a")
        data.Add("a -high-> inv", PulsingEdge.Create "a" High "inv")
        data.Add("a -high-> con", PulsingEdge.Create "a" High "con")
        data.Add("inv -low-> b", PulsingEdge.Create "inv" Low "b")
        data.Add("con -high-> output", PulsingEdge.Create "con" High "output")
        data.Add("b -high-> con", PulsingEdge.Create "b" High "con")
        data.Add("con -low-> output", PulsingEdge.Create "con" Low "output")

        data

    [<Theory>]
    [<MemberData(nameof(PulsingEdgeData))>]
    let DeserializePulsingEdge(source: string, expected: PulsingEdge) = 
        let actual = PulsingEdge.Parse source

        Assert.Equal(expected, actual)
