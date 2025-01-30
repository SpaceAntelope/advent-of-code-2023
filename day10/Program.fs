open System.IO
open Common
open System.Text

//  ([Up;Rt;Dn;Lt] ,[Up;Rt;Dn;Lt]) ||> List.allPairs |> List.iter(fun (x,y)->printfn "| %A, %A -> " x y)

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
        member x.Opposite = 
            match x with Up -> Dn | Lt -> Rt | Dn -> Up | Rt -> Lt
        member x.RotateRight =
            match x with Up -> Rt | Rt -> Dn | Dn -> Lt | Lt -> Up
        member x.RotateLeft =
            match x with Up -> Lt | Lt -> Dn | Dn -> Rt | Rt -> Up

//([Hor;Ver;TLt;TRt;BLt;BRt],[Hor;Ver;TLt;TRt;BLt;BRt]) ||> List.allPairs |> List.iter(fun (x,y)->printfn "| %A, %A -> " x y)
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
        
        // member x.Edges =
        //     match x with
        //     | Ver -> Lt,Rt
        //     | Hor -> Up,Dn
        //     | TRt -> Ver, Rt


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
        path.Add(current)
        //printfn "%04d %A" depth current
        let row,col = current
        visited.[row,col] <- true
        
        match (next current) with
        | None -> () //List.empty
        | Some cell -> 
            search cell (depth + 1)
            //cell::(search cell (depth + 1))
            
    let start = matrix |> Matrix.findCell (fun tile -> tile = Tile.Start)

    search start 0

    path

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
        // "./input/puzzle.input"
    ]
    |> List.map parse
    |> List.iter (fun matrix -> 
        let path = followPipe matrix
        printfn "Path size = %d" path.Count
        printPath matrix path
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

// pathDisplay()



let countEnclosed path =    
    printfn "Processing %s" path
    let matrix = parse path
    let rows,cols = matrix |> Matrix.size
    let pipeline = followPipe matrix
    let pipeSet = pipeline |> Set.ofSeq

    // printPath matrix pipeline

    let tileFromStart = 
        let start = matrix |> Matrix.findCell (fun x -> x = Start)
        let secondRow,secondCol = pipeline.[1]
        let lastRow, lastCol = pipeline |> Seq.last
        let dir1 = Dir.From((lastRow,lastCol),start )
        let dir2 = Dir.From(start, (secondRow, secondCol))

        match dir1, dir2 with
        | Up, Up -> Ver
        | Dn, Dn -> Ver
        | Rt, Rt -> Hor
        | Lt, Lt -> Hor
        | Up, Lt -> TRt
        | Rt, Dn -> TRt
        | Lt, Dn -> TLt
        | Up, Rt -> TLt
        | Dn, Lt -> BRt
        | Rt, Up -> BRt
        | Dn, Rt -> BLt
        | Lt, Up -> BLt
        | x, y -> failwithf "x - %A -> y - %A -> z shouldn't be possible" x y
        // | Up, Dn -> 
        // | Rt, Lt -> 
        // | Dn, Up -> 
        // | Lt, Rt -> 


    let isOutOfBounds = Matrix.isOutOfBoundsFactory matrix

    let arrayNotInPipe (vec: (int * int) list)  =
        vec |> List.forall (fun cell -> pipeSet |> Set.contains cell |> not)

    let mutable enclosed = Set.empty<int*int>

    let isEnclosable cell =
        isOutOfBounds (fst cell)  (snd cell) |> not
        && pipeSet |> Set.contains cell |> not
        && enclosed |> Set.contains cell |> not
    
    let rec gatherEnclosed start = 
        // printfn "Start of enclosure: %A" start        
        let row,col = start
        [ row+1,col;row-1,col;row,col+1;row,col-1]
        |> List.filter isEnclosable
        // |> General.tee $"currently enclosed {enclosed.Count}"
        |> List.iter (fun cell -> 
            enclosed <- enclosed |> Set.add cell
            gatherEnclosed cell)

    let outwardLookingPipePiece = 
        pipeline
        |> Seq.choose (fun (row,col) -> 
            match matrix.[row,col] with
            | Hor when [0..row-1] |> List.map (fun r -> r,col) |> arrayNotInPipe -> Some (row,col,Up)
            | Hor when [row+1 .. rows-1] |> List.map (fun r -> r,col) |> arrayNotInPipe -> Some (row,col,Dn)
            | Ver when [0..col-1] |> List.map (fun c -> row,c) |> arrayNotInPipe -> Some (row,col,Lt)
            | Ver when [col+1 .. cols-1] |> List.map (fun c -> row,c) |> arrayNotInPipe -> Some (row,col,Rt)
            | _ -> None)
        |> Seq.head
    
    
    let mutable prevCell, prevInwardFace = 
        outwardLookingPipePiece 
        |> fun (row,col,dir) -> (row,col), dir.Opposite

    let mutable index = pipeline |> Seq.findIndex (fun cell -> cell = prevCell)
    
    for i in 1..pipeline.Count-1 do
        let idx = (i + index) % pipeline.Count
        let row,col = pipeline.[idx]
        let dirOfEntry = Dir.From(prevCell,(row,col))
        let tile = 
            let tile = matrix.[row,col]
            if tile = Start 
            then tileFromStart 
            else tile

        let inwardFace = 
            match tile, dirOfEntry with
            | Ver, _ | Hor, _ -> prevInwardFace
            | TRt, Rt -> prevInwardFace.RotateRight
            | TRt, Up -> prevInwardFace.RotateLeft
            | TLt, Lt -> prevInwardFace.RotateLeft
            | TLt, Up -> prevInwardFace.RotateRight
            | BLt, Dn -> prevInwardFace.RotateLeft
            | BLt, Lt -> prevInwardFace.RotateRight
            | BRt, Dn -> prevInwardFace.RotateRight
            | BRt, Rt -> prevInwardFace.RotateLeft
            | x -> failwithf "%A is not a pipe" x

        let inwardCell = 
            match inwardFace with
            | Up -> row - 1, col
            | Dn -> row + 1, col
            | Lt -> row, col - 1
            | Rt -> row, col + 1

        if isEnclosable inwardCell
        then 
            enclosed <- enclosed |> Set.add inwardCell
            gatherEnclosed inwardCell

        // printfn "Step: %d" i
        // printfn "Cell %A Inward cell %A " (row,col) inwardCell
        // printfn "DirOfEntry %A InwardDir %A" dirOfEntry inwardFace
        // printfn "%d" enclosed.Count

        prevCell <- row,col
        prevInwardFace <- inwardFace

        for (row,col) in enclosed do
            matrix.[row,col] <- Enclosed
        
        //printPath matrix []
        // printfn "Enclosed count %d" enclosed.Count
    
    // printPath matrix pipeline

    enclosed.Count



"./input/puzzle.example1"
|> countEnclosed
|> Assertions.shouldBe 4

"./input/puzzle.example5"
|> countEnclosed
|> Assertions.shouldBe 4

"./input/puzzle.example2"
|> countEnclosed
|> Assertions.shouldBe 8

"./input/puzzle.example3"
|> countEnclosed
|> Assertions.shouldBe 10

"./input/puzzle.input"
|> countEnclosed
|> printfn "There are %d tiles enclosed by the loop."
