open System.IO
open Common

type Dir = Lt | Rt | Up | Dn
    with 
        static member From(cell1: int*int, cell2: int*int) =
            let row1,col1 = cell1
            let row2,col2 = cell2
            match row2-row1,col2-col1 with
            | rowDiff, colDiff when rowDiff = 0 && colDiff > 0 -> Rt
            | rowDiff, colDiff when rowDiff = 0 && colDiff < 0 -> Lt
            | rowDiff, colDiff when rowDiff > 0 && colDiff = 0 -> Dn
            | rowDiff, colDiff when rowDiff < 0 && colDiff = 0 -> Up
            | x -> failwithf "%A -> %A = %A bad diff" x (row1,col1) (row2,col2)

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


let followPipe matrix =
    let isOutOfBounds = Matrix.isOutOfBoundsFactory matrix
    let start = matrix |> Matrix.findCell (fun tile -> tile = Tile.Start)
    let mutable visited = Set.empty<int*int>
    let next cell = 
        let row,col = cell
        [   row + 1, col
            row - 1, col
            row, col + 1
            row, col - 1 ]
        |> List.filter (fun (row,col) -> not (isOutOfBounds row col) && not (visited |> Set.contains((row,col))) )
        |> List.tryFind (fun candidate ->
                let row1, col1 = cell
                let row2, col2 = candidate
                let dir = Dir.From(cell, candidate)
                
                match matrix.[row1,col1], matrix.[row2,col2], dir with
                | Hor, Hor, Rt -> true // ──
                | Hor, Hor, Lt -> true // ──
                
                | Hor, BRt, Rt -> true // ─┘
                | Hor, TRt, Rt -> true // ─┐
                | BLt, Hor, Rt -> true // └─
                | BLt, BRt, Rt -> true // └┘
                | BLt, TRt, Rt -> true // └┐
                | TLt, Hor, Rt -> true // ┌─
                | TLt, BRt, Rt -> true // ┌┘
                | TLt, TRt, Rt -> true // ┌┐
                
                | BRt, Hor, Lt -> true // ─┘
                | TRt, Hor, Lt -> true // ─┐
                | Hor, BLt, Lt -> true // └─
                | BRt, BLt, Lt -> true // └┘
                | TRt, BLt, Lt -> true // └┐
                | Hor, TLt, Lt -> true // ┌─
                | BRt, TLt, Lt -> true // ┌┘
                | TRt, TLt, Lt -> true // ┌┐

                //  │
                //  │
                | Ver, Ver, Dn -> true
                | Ver, Ver, Up -> true
                
                //  │
                //  └
                | Ver, BLt, Dn -> true
                //  │
                //  ┘
                | BRt, TLt, Dn -> true
                //  ┐
                //  │
                | TRt, Ver, Dn -> true
                //  ┐
                //  └
                | TRt, BLt, Dn -> true
                //  ┐
                //  ┘
                | TRt, BRt, Dn -> true
                //  ┌
                //  │
                | TLt, Ver , Dn-> true
                //  ┌
                //  └
                | TLt, BLt, Dn -> true
                //  ┌
                //  ┘
                | TLt, BRt, Dn -> true
                
                | BLt, Ver, Up -> true
                | TLt, BRt, Up -> true
                | Ver, TRt, Up -> true
                | BLt, TRt, Up -> true
                | BRt, TRt, Up -> true
                | Ver, TLt, Up-> true
                | BLt, TLt, Up -> true
                | BRt, TLt, Up -> true
                | _ -> false)
                                
    let rec search current = 
        visited <- visited |> Set.add current
        [ 
            yield! 
                match (next current) with
                | None -> []
                | Some cell -> [Some current; next cell]
        ] |> List.choose id
    
    search start

let tileSet = [
    Ver
    Hor
    BLt
    BRt
    TRt
    TLt]

printfn "Horizontal"
tileSet 
|> List.allPairs tileSet 
|> List.iter (fun (t1,t2) -> 
    printfn "| %A, %A -> true // %c%c" t1 t2 t1.AsAscii t2.AsAscii)

printfn "Vertical"
tileSet 
|> List.allPairs tileSet 
|> List.iter (fun (t1,t2) -> 
    printfn "//  %c" t1.AsAscii
    printfn "//  %c" t2.AsAscii    
    printfn "| %A, %A -> true" t1 t2
     )
