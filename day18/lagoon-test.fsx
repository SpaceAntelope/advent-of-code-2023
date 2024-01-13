#load "./lagoon-common.fsx"
open System
open ``Lagoon-common``

let testData =
    @"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"


let trenchArea (path:Path) = 
    let (rowCount,colCount) = pathAreaDimensions path
    Array2D.init rowCount colCount (fun row col -> if path |> Array.contains (row,col) then '#' else '.')

let fillTrench (trench: Trench) =
    
    let rowCount = trench |> Array2D.length1
    let colCount = trench |> Array2D.length2

    for row in 0..rowCount-1 do
        let mutable sentinel = false
        for col in 0..colCount-1 do
            if trench[row,col] = '#' && (col = colCount-1 || trench[row,col+1] <> '#')
            then sentinel <- not sentinel
            else if sentinel = true
            then trench[row,col] <- '#'
    
    trench

// testData
// |> parse
// |> path 
// |> trenchArea
// |> fun trench -> printfn "%A\n" trench; trench
// |> fillTrench
// |> fun table -> table |> Seq.cast<char> |> Seq.countBy id
// |> printfn "%A"

let pathTest = 
    let expected = 38
    testData
    |> parse
    |> path    
    |> Array.length
    |> function 
    | actual when actual = expected -> printfn "Path creation test passed"
    | actual -> printfn $"Path creation test failed. Expected path length {expected} but got {actual}"


let testDigTrench1 =
    let expected = 62
    testData
    |> parse
    |> path    
    |> fillPath
    |> function 
    | actual when actual = expected -> printfn "Path fill test 1 passed"
    | actual -> printfn $"Path fill test 1 failed. Expected path length {expected} but got {actual}"

let testDigTrench2 =
    let expected = 62
    testData
    |> parse
    |> path    
    |> rebasePath
    |> trenchArea
    |> fillTrench
    |> Seq.cast<char>
    |> Seq.filter (fun c -> c = '#')
    |> Seq.length
    |> function 
    | actual when actual = expected -> printfn "Path fill test 2 passed"
    | actual -> printfn $"Path fill test 2 failed. Expected path length {expected} but got {actual}"
