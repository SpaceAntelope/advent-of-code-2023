open System

type Direction = D | U | L | R
type Path = (int*int)[]
type Trench = char array2d

let DirIndex = [ 'D', D; 'U', U; 'L', L; 'R', R ] |> readOnlyDict

type Instruction =
    { Dir: Direction
      Steps: int
      Color: string }

let tee input = printfn "%A" input; input

let parse (data: string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map (fun line ->
        let parts = line.Split(' ')

        { Dir = DirIndex[parts[0][0]]
          Steps = int parts[1]
          Color = parts[2].Trim([| '('; ')'; '#' |]) })

let parseExtended (data: Instruction ) =
    {
        Dir = 
            match int <| data.Color.Chars(data.Color.Length-1).ToString() with
            | 0 -> R | 1 -> D | 2 -> L | 3 -> U | x -> failwith $"Unknown direction {x}"
        Color = String.Empty
        Steps = Convert.ToInt32(data.Color.Substring(0,5), 16)
    }

let path (instructions: Instruction[]) : Path  =
    instructions
    |> Array.scan (fun state instr -> 
        let (row,col) = state |> Array.last
        [|1..instr.Steps|]
        |> Array.map (fun i -> 
                match instr.Dir with
                | D -> (row+i, col)
                | U -> (row-i, col)
                | R -> (row, col+i)
                | L -> (row, col-i))) [|(0,0)|]
    |> Array.collect id
    |> Array.skip 1 // (0,0) appears both first and last

let nodes (instructions : Instruction[]) : Path =
    instructions
    |> Array.scan (fun (row,col) instr -> 
            match instr.Dir with
            | D -> (row+instr.Steps, col)
            | U -> (row-instr.Steps, col)
            | R -> (row, col+instr.Steps)
            | L -> (row, col-instr.Steps)
        ) (0,0)

let pathAreaDimensions (path: Path) =
    path |> Array.map fst |> Array.max |> (+)1,
    path |> Array.map snd |> Array.max |> (+)1

let rebasePath (path:Path) =
    let rows = path |> Array.map fst
    let cols = path |> Array.map snd
    let rowMin, rowMax = rows |> Array.min, rows |> Array.max
    let colMin, colMax = cols |> Array.min, rows |> Array.max


    let rowOffset = if rowMin < 0 then - rowMin else 0
    let colOffset = if colMin < 0 then - colMin else 0

    if rowOffset > 0 || colOffset > 0 
    then path |> Array.map (fun (row,col) -> row + rowOffset, col + colOffset)
    else path

let fillPath (path:Path) =
    path
    |> Array.groupBy fst
    |> Array.map (fun (row,points) -> 
            points 
            |> Array.sortBy snd 
            |> Array.map snd
            |> Array.fold (fun (sum, sentinel,prevCol) currentCol ->
                if prevCol = currentCol - 1 
                then (sum, sentinel, currentCol)
                else if sentinel 
                then (sum + currentCol - prevCol - 1, not sentinel, currentCol)
                else (sum, not sentinel, currentCol)
            ) (0,false,-2)
            |> fun (sum,_,_) -> sum)
            // |> Array.chunkBySize 2 
            // |> Array.map (fun pair -> 
            //     if pair.Length = 2 
            //     then 
            //         let min = snd pair[0]
            //         let max = snd pair[1]
            //         printfn $"{i} {max-min-1} %A{pair}"
            //         max - min - 1
            //     else `
            //         printfn $"{i} %A{pair}"
            //         0)
    |> Array.reduce (+)
    |> fun interior -> interior + path.Length

let trenchArea (path:Path) = 
    let (rowCount,colCount) = pathAreaDimensions path
    Array2D.init rowCount colCount (fun row col -> if path |> Array.contains (row,col) then '#' else '.')

let fillTrench (trench: Trench) =
    
    let rowCount = trench |> Array2D.length1
    let colCount = trench |> Array2D.length2

    let filledTrench = Array2D.init rowCount colCount (fun row col -> trench[row,col])

    for row in 0..rowCount-1 do
        let mutable sentinel = false
        let mutable trenchStart = -1
        let mutable trenchStop = -1
        for col in 0..colCount-1 do
            if trench[row,col] = '#' 
            then 
                if col = 0 || trench[row,col-1] <> '#' 
                then trenchStart <- col
                
                if (col = colCount-1 || trench[row,col+1] <> '#')
                then trenchStop <- col

                if (row > 0 && row < rowCount-1 && trenchStop = col) && (trench[row-1,trenchStart] = trench[row+1,trenchStop] || trench[row+1,trenchStart] = trench[row-1,trenchStop])
                then sentinel <- not sentinel            
            else 
                if sentinel then filledTrench[row,col] <- '#'
    
    filledTrench


module Fill = 
    type Line = { Start : int*int; Stop: int*int }
    type State = { PreviousRow : Line[]; Area: int }

    // let fillNodes (path: Path) =
    //     let rows = 
    //         path
    //         |> Array.sort
    //         |> Array.groupBy fst // group to rows
    //         |> Array.map snd
        
    //     let mutable previousRow: (int*int) array = [||]
    //     for row in rows do
    //         for point in previousRow do
    //             let rowIndex,colIndex  = point // bottonLeft
    //             let top = 
    //                 previousRow 
    //                 |> Array.tryFind (fun (r,c) -> c = colIndex)
    //             let right = 
    //                 row 
    //                 |> Array.filter (fun (r,c) -> c > colIndex)
    //                 |> Array.tryHead
    //             let left = 
    //                 row 
    //                 |> Array.filter (fun (r,c) -> c < colIndex)
    //                 |> Array.tryLast
 

let str (table: char array2d) = 
    let rowCount = table |> Array2D.length1

    [0..rowCount-1]
    |> List.map (fun rowIndex -> table[rowIndex,*] |> Array.fold (sprintf "%s %c") "")
    |> List.reduce (sprintf "%s\n%s")

let html (table: char array2d) =
    let rowCount = table |> Array2D.length1

    seq {
        yield "
        <style>
            * {
                padding: 0;
                margin: 0;
            }

            table, th, td {
                border: 1px solid black;
                border-spacing: 0;
            }

            td > * {
                min-height: 2px;
                min-width: 2px;
            }
        </style>
        "
        yield "<table>"
        yield! [0..rowCount-1]
            |> List.map (fun rowIndex -> 
            [
                "<tr>"
                table[rowIndex,*] |> Array.fold (fun state current -> match current with '.' -> $"{state} <td><div ></div></td>" | '#' -> $"{state} <td><div style='background-color: green;'></div></td>") ""
                "</tr>"
            ])
            |> List.collect id     
        yield "</table>"    
    } |> String.concat "\n"
   