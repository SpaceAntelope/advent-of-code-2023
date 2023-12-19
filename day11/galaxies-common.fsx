open System
open System.Text
open System.Collections.Generic

let stringifyArray2D<'TValue> (table: 'TValue array2d) =
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    let sb = StringBuilder()
    
    let maxValueLength = 
        table
        |> Seq.cast<'TValue>
        |> Seq.map _.ToString()
        |> Seq.maxBy _.Length
        |> _.Length

    for row in [0..rowCount-1] do
        for col in [0..colCount-1] do
            sb.Append(table[row,col].ToString().PadLeft(maxValueLength + 1))
            |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()


let invert (table: 'a array2d) = 
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    Array2D.init colCount rowCount (fun row col -> table[col,row])


let parse (data: string) = 
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> array2D

let expand (table: char array2d) =

    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    
    let duplicateEmptyArray (line: char seq) =
        line 
        |> Seq.forall (fun c -> c = '.')
        |> function
        | true -> seq [line;line]
        | false -> seq [line]

    [|0..rowCount-1|]
    |> Seq.map (fun rowIndex -> table[rowIndex,*])
    |> Seq.collect duplicateEmptyArray
    |> array2D
    |> fun arr -> 
        [0..colCount-1] 
        |> Seq.map (fun colIndex -> arr[*,colIndex])
    |> Seq.collect duplicateEmptyArray
    |> array2D
    |> invert


let numberGalaxies (table: char array2d) =
    let mutable cnt = 0
    table 
    |> Array2D.map (
        function 
        | '.' -> "." 
        | '#' -> 
            cnt <- cnt + 1
            cnt.ToString()
        | x -> failwith $"Unknown astronomical data {x}"
     )        

let manhattanDistance (src: int*int) (dst: int*int) = 
    let row1,col1 = src
    let row2, col2 = dst
    Math.Abs(row2-row1) + Math.Abs(col2-col1)        

let expandedManhattanDistance (src: int*int) (dst: int*int) (emptyLineIndices: int[]) (emptyColIndices: int[]) (expansionFactor: int)=
    let row1, col1 = src
    let row2, col2 = dst

    let minRow, maxRow = if row1 > row2 then row2,row1 else row1,row2
    let minCol, maxCol = if col1 > col2 then col2,col1 else col1,col2
    let emptyRowCount = 
        emptyLineIndices 
        |> Array.filter (fun i -> i > minRow && i < maxRow ) 
        |> Array.length 
    let emptyColCount = 
        emptyColIndices 
        |> Array.filter (fun i -> i > minCol && i < maxCol ) 
        |> Array.length 
                
    let verticalDistance = maxRow - minRow + emptyRowCount * expansionFactor - emptyRowCount
    let horizontalDistance = maxCol - minCol + emptyColCount * expansionFactor - emptyColCount
    
    verticalDistance + horizontalDistance

let numberedPoints (table: char array2d) = 
    table
    |> numberGalaxies
    // |> fun x -> IO.File.WriteAllText("./output.txt", stringifyArray2D x); x
    |> Array2D.mapi (fun row col value -> row,col,value.ToString())
    |> Seq.cast<int*int*string>
    |> Seq.filter (fun (_,_,value)-> value <> ".")
    |> Seq.map(fun (r,c,v) -> int v, (r,c))
    |> readOnlyDict

let emptyRows (table: char array2d) =
    let rowCount = table |> Array2D.length1
    [|0..rowCount-1|]
    |> Array.map (fun rowIndex -> rowIndex, table[rowIndex,*])        
    |> Array.filter (snd>>(Array.forall (fun x -> x = '.')))
    |> Array.map fst 

let emptyColumns (table: char array2d) =
    let colCount = table |> Array2D.length2
    [|0..colCount-1|]
    |> Array.map (fun colIndex -> colIndex, table[*,colIndex])        
    |> Array.filter (snd>>(Array.forall (fun x -> x = '.')))
    |> Array.map fst

let allDistancePairs (numberedPoints: IReadOnlyDictionary<int,int*int>) =
    let gs = numberedPoints.Keys |> Array.ofSeq
    let ln = gs |> Seq.length
    
    seq {
        for i in 0..ln-1 do
            for j in i..ln - 1 do
                if i <> j 
                then yield gs[i],gs[j]
    }

let allDistancePairs2 (numberedPoints: IReadOnlyDictionary<int,int*int>) = 
    Seq.allPairs numberedPoints.Keys numberedPoints.Keys
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.map (fun (x,y) -> [|x;y|] |> Array.sort )
    |> Set.ofSeq
