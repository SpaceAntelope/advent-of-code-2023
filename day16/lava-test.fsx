#load "./lava-common.fsx"

open ``Lava-common``

open System
open System.Collections.Generic
let testData = @"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."

let testCountEnergized = 
    let expected = 46
    testData
    |> parse
    |> tripTheLight ToRight (0,0)
    |> fun x -> printfn "%A" x; x
    |> countEnergized
    |> function
    | actual when actual = expected -> printfn "Energized count test passed."
    | actual -> failwith $"Energized count test failed. Expected: {expected} but got {actual}"


let testStartingVectorCount = 
    let table =
        testData
        |> parse   

    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2

    let expected = rowCount * 2 + colCount * 2

    table    
    |> startingVectors
    |> Array.length
    |> function 
    | actual when actual = expected -> printfn "Starting vector count test passed."
    | actual  -> failwith $"Starting vector count test failed. Expected {expected} but got {actual}."

let testMaxEnergization = 
    let expected = 51
    let table = 
        testData
        |> parse   

    table            
    |> startingVectors
    |> Array.map (fun (dir, (row,col)) ->
        table
        |> tripTheLight dir (row,col)
        |> countEnergized)
    |> Array.max
    |> function
    | actual when actual = expected -> printfn "Max energization test passed."
    | actual -> printfn $"Max energization test failed. Expected {expected} but got {actual}"
    
