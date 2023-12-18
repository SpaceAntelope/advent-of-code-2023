#load "./pipes-common.fsx"

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open ``Pipes-common``



let testDataSquare = 
    @"
.....
.S-7.
.|.|.
.L-J.
....."
    |> parse




[   { Tile = 'S';Row = 1; Col = 1 }, { Tile = '|';Row = 2; Col = 1 }, true
    { Tile = '-';Row = 1; Col = 2 }, { Tile = '7';Row = 1; Col = 3 }, true
    { Tile = '7';Row = 1; Col = 3 }, { Tile = '|';Row = 2; Col = 3 }, true
    { Tile = '|';Row = 2; Col = 1 }, { Tile = 'S';Row = 1; Col = 1 }, true 
    { Tile = 'S';Row = 42; Col = 8 }, { Tile = '-';Row = 41; Col = 8 }, false 
    ]
|> List.filter (fun (src, dst, expected) -> (isValidConnection src dst) <> expected)
|> function
| [] -> printfn "All connection test cases ok"
| x -> failwith $"Connection test failed at {x}"

let expectedFurthestDistance = 4

@"
.....
.S-7.
.|.|.
.L-J.
....."
|> parse
|> findFurthestDistance
|> function 
| actual when actual <> expectedFurthestDistance -> printfn $"Expected distance of {expectedFurthestDistance} but found {actual}"
| _ -> printfn "Find furthest distance test 1 passed"


let expectedFurthestDistance2 = 8

@"
..F7.
.FJ|.
SJ.L7
|F--J
LJ..."
|> parse
|> findFurthestDistance
|> function 
| actual when actual <> expectedFurthestDistance2 -> printfn $"Expected distance of {expectedFurthestDistance} but found {actual}"
| _ -> printfn "Find furthest distance test 2 passed"
