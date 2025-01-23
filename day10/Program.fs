open System.IO
open Common

type Dir = N | E | W | S
type Tile = 
    | Hor | Ver | TLt | TRt | BLt | BRt | Ground | Start | Enclosed
    with
        static member From(c:char) = 
            match c with 
            | '|' -> Ver
            | '-' -> Hor
            | 'L' -> BLt
            | 'J' -> BRt
            | '7' -> TRt
            | 'F' -> TLt
            | '.' -> Ground
            | 'S' -> Start
            | 'I' -> Enclosed
            | x -> failwithf "Unexpected tile type '%c'" x
        member x.AsAscii =
            match x with 
            | Ver -> '│'
            | Hor -> '─'
            | BLt -> '└'
            | BRt -> '┘'
            | TRt -> '┐'
            | TLt -> '┌'
            | Ground -> '▒'
            | Start -> '✿'
            | Enclosed -> '█'

type Matrix = Tile array2d

let parse path = 
    path 
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> Array.map (Array.map Tile.From)
    |> array2D

let printTiles (matrix: Tile array2d) = 
    let rows,cols = Common.Matrix.size matrix
    Array2D.init rows cols (fun row col -> matrix.[row,col] |> _.AsAscii)
    |> Common.Matrix.simplePrintUnspaced
    printfn ""

[   "./input/puzzle.example1"
    "./input/puzzle.example2"
    "./input/puzzle.example3" 
    "./input/puzzle.example4" 
    "./input/puzzle.example5"  ]
|> List.map parse
|> List.iter printTiles

// let followPipe matrix =
//     let isOutOfBounds = Matrix.isOutOfBoundsFactory matrix
//     let start = matrix |> Matrix.findCell (fun tile -> tile = S)
//     let visited = Set.empty<int*int>
//     let next cell = 
        
//         [   row + 1, col
//             row - 1, col
//             row, col + 1
//             row, col - 1 ]
//         |> List.filter (fun (row,col) -> not isOutOfBounds row col && not visited |> Set.contains((row,col)) )
//         |> List.find (fun candidate ->
//                 let row1, col1 = cell
//                 let row2, col2 = candidate
//                 match matrix.[row1,col1], matrix.[row2,col2] with
//                 | Hor, Hor
//                 | Ver, Ver -> true
//                 | TLt, TRt -> true
//                 | BLt, BRt -> true
//                 | TLt

//         )
//         match matrix.[row,col], 
//     let rec search current = 

let tileSet = [
    Ver
    Hor
    BLt
    BRt
    TRt
    TLt]

tileSet 
|> List.allPairs tileSet 
|> List.iter (fun (t1,t2) -> 
    printfn "| %A, %A -> true" t1 t2
    printfn "%c%c" t1.AsAscii t2.AsAscii
    printfn "%c\n" t2.AsAscii    
     )
