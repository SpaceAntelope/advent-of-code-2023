#load "./rocks-common.fsx"

open System
open System.IO
open System.Collections.Generic
open ``Rocks-common``

let testData = @"
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."


let testTiltNorth = 
    let expectedTable = parse @"OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#...." 
    let expectedScore = 136

    testData
    |> parse
    |> tiltNorth
    |> function 
    | actual when expectedTable = actual -> printfn "Tilt north test passed"; actual
    | actual -> failwith $"Tilt north test failed. Expected \n%A{expectedTable}\n but got \n%A{actual}\n"
    |> score
    |> function
    | actual when expectedScore = actual -> printfn "Score calcualtion passed"
    | actual -> failwith $"Score calculation failed: Expected {expectedScore} but got {actual}."