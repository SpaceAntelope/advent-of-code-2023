#load "./galaxies-common.fsx"


open System
open System.Text
open System.Collections.Generic
open ``Galaxies-common``

let testData = 
    @"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."


let expansionTest = 
    let expectedExpanded = 
        @"
    ....#........
    .........#...
    #............
    .............
    .............
    ........#....
    .#...........
    ............#
    .............
    .............
    .........#...
    #....#.......
    " |> parse

    let actualExpanded = 
        testData
        |> parse 
        |> expand 

    if expectedExpanded <> actualExpanded 
    then failwith $"Expansion test: Expected \n{stringifyArray2D expectedExpanded} but got \n{stringifyArray2D actualExpanded}"
    else printfn "Expansion test: Passed!"

    actualExpanded
    |> numberGalaxies
    |> stringifyArray2D
    |> printfn "%s"

let testDistances =
    let points = 
        testData
        |> parse
        |> expand
        |> numberedPoints
        
    let distanceTestCases = [   
            1, 7, 15
            3, 6, 17
            8, 9, 5 ]

    distanceTestCases
    |> List.iter(fun (g1,g2,expected) -> 
        match manhattanDistance points[g1] points[g2] with
        | actual when actual = expected -> printfn $"Distance test {g1} -> {g2} passed"
        | actual -> failwith $"Distance test {g1} -> {g2} failed, expected {expected} but got {actual}")

let pairCountTest = 
    let expected = 36

    let points = 
        testData
        |> parse
        |> expand
        |> numberedPoints
    
    let pairs = 
        allDistancePairs points 
        |> Seq.map (fun (x,y) -> [|x;y|] |> Array.sort )
        |> Array.ofSeq
    
    let pairs2 = 
        allDistancePairs2 points 
        |> Array.ofSeq

    if pairs.Length = pairs2.Length && pairs.Length = 36 
    then printfn "Pair count test passed"
    else failwith $"Expected {expected} unique pairs but got {pairs.Length} and {pairs2.Length}"

let testSumOfDistances =
    let expected = 374

    let points = 
        testData
        |> parse
        |> expand
        |> numberedPoints

    points 
    |> allDistancePairs
    |> Seq.sumBy (fun (p1,p2) -> manhattanDistance points[p1] points[p2])
    |> function
    | actual when actual = expected -> printfn "Total distance test passed."
    | actual -> failwith $"Expected total distance {expected} but got {actual}"