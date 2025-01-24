open System.IO
open Common
open System.Text

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
        member x.AsAscii2 =
            match x with 
            | Ver -> '║'
            | Hor -> '═'
            | BLt -> '╚'
            | BRt -> '╝'
            | TRt -> '╗'
            | TLt -> '╔'
            | Ground -> '▒'
            | Start -> '✿'
            | Enclosed -> '█'

type Matrix = Tile array2d
type Cell = int*int

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

let printPath (matrix: Tile array2d) (path: Cell seq) =
    let rows,cols = Matrix.size matrix
   
    let iconRules = 
        path 
        |> Seq.groupBy (fun (r,c) -> matrix.[r,c])
        |> Seq.map(fun (c,grp) -> grp |> Set.ofSeq, c.AsAscii2)
    let asciiMatrix = 
        Array2D.init rows cols (fun row col -> matrix.[row,col] |> _.AsAscii)
    
    Matrix.printBase asciiMatrix iconRules

let followPipe (matrix: Tile array2d) =
    let (rows,cols) = Matrix.size matrix
    let isOutOfBounds = Matrix.isOutOfBoundsFactory matrix
    let visited = Array2D.create rows cols false
    let next cell = 
        let row,col = cell
        [   row + 1, col
            row - 1, col
            row, col + 1
            row, col - 1 ]
        |> List.filter (fun (row,col) -> not (isOutOfBounds row col) && not (visited.[row,col]) )
        |> List.tryFind (fun candidate ->
                
                let row1, col1 = cell
                let row2, col2 = candidate
                let dir = Dir.From(cell, candidate)
                
                // printf "%c - %A -> %c " (matrix.[row1,col1].AsAscii) dir (matrix.[row2,col2].AsAscii)

                match matrix.[row1,col1], matrix.[row2,col2], dir with
                | Start, Ground, _ -> false
                | Start, Start, _ -> false
                | Start, _, _ -> true

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

                //  │
                //  └
                | Ver, BLt, Dn -> true

                //  │
                //  ┘
                | Ver, BRt, Dn -> true


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
                | TLt, Ver, Dn -> true

                //  ┌
                //  └
                | TLt, BLt, Dn -> true

                //  ┌
                //  ┘
                | TLt, BRt, Dn -> true

                | Ver, Ver, Up  -> true
                | BLt, Ver, Up  -> true
                | BRt, Ver, Up  -> true
                | Ver, TRt, Up  -> true
                | BLt, TRt, Up  -> true
                | BRt, TRt, Up  -> true
                | Ver, TLt, Up  -> true
                | BLt, TLt, Up  -> true
                | BRt, TLt, Up  -> true
                | _ -> false
                // |> General.tee ""
                )

    let path = ResizeArray<int*int>()
                                
    let rec search current depth  = 
        // path.Add(current)
        //printfn "%04d %A" depth current
        let row,col = current
        visited.[row,col] <- true
        
        match (next current) with
        | None -> List.empty
        | Some cell -> 
            // search cell (depth + 1)
            cell::(search cell (depth + 1))
            
    let start = matrix |> Matrix.findCell (fun tile -> tile = Tile.Start)

    search start 0

    // path

let compatibleTilesHelper() =
        let sb = StringBuilder()
        
        let str (msg: string) = sb.AppendLine(msg) |> ignore

        let tileSet = [
            Ver
            Hor
            BLt
            BRt
            TRt
            TLt]

        str "Horizontal"
        tileSet 
        |> List.allPairs tileSet 
        |> List.filter (fun (t1,t2) -> t1 <> Ver && t2 <> Ver)
        |> List.iter (fun (t1,t2) -> 
            sprintf "| %A, %A -> true // %c%c" t1 t2 t1.AsAscii t2.AsAscii
            |> str)

        str "Vertical"
        tileSet 
        |> List.allPairs tileSet 
        |> List.filter (fun (t1,t2) -> t1 <> Hor && t2 <> Hor)
        |> List.iter (fun (t1,t2) -> 
            sprintf "//  %c" t1.AsAscii |> str
            sprintf "//  %c" t2.AsAscii |> str
            sprintf "| %A, %A -> true\n" t1 t2 |> str)

        File.WriteAllText("directions.temp.txt", sb.ToString())

let pathDisplay() =
    

    [   "./input/puzzle.example1"
        "./input/puzzle.example2"
        "./input/puzzle.example3" 
        "./input/puzzle.example4" 
        "./input/puzzle.example5"
        "./input/puzzle.input"
    ]
    |> List.map parse
    |> List.iter (fun matrix -> 
        let path = followPipe matrix
        printfn "Path size = %d" path.Length
        // printPath matrix path
        // ()
        )



// let findEnclosed path matrix = 

// [   "./input/puzzle.example1"
//     "./input/puzzle.example2"
//     "./input/puzzle.example3" 
//     "./input/puzzle.example4" 
//     "./input/puzzle.example5"  ]
// |> List.map parse
// |> List.iter printTiles

pathDisplay()