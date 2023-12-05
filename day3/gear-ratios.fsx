open System
open System.IO
open System.Text.RegularExpressions

let parse (str: string) = 
    str.Split('\n') 
    |> Array.map _.Trim().ToCharArray() 
    |> array2D

let isSymbol (value : char) =  value <> '.' && (value < '0' || value > '9')

let isNumber (value: char) = value >= '0' && value <= '9'

let area (row:int) (col:int) = [
    (row-1,col-1);(row-1,col);(row-1,col+1);
    (row,col-1);(*(row,col);*)(row,col+1)
    (row+1,col-1);(row+1,col);(row+1,col+1);
    ]

let isNeighbor (point1 : int*int) (point2: int*int) =
    let (row1,col1) = point1
    let (row2,col2) = point2

    Math.Abs(row1-row2) <=1 && Math.Abs(col1-col2) <= 1

let numFromPoint(point: int*int) (table: char array2d) = 
    let (row,col) = point
    let strRow = table[row,*] |> String

    Regex.Matches(strRow, "\\d+")
    |> Seq.cast<Match>
    |> Seq.tryFind (fun m -> m.Index <= col && col <= (m.Index + m.Length - 1) )
    |> Option.map (fun m -> m.Index, m.Value)

let components (table: char array2d) =
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    
    [
        for row in 0..rowCount-1 do
            for col in 0..colCount-1 do
                if isSymbol table[row,col] 
                then
                    yield! area row col
                    |> List.where (fun (row,col) -> row >= 0 && col >= 0 && row < rowCount && col < colCount && isNumber table[row,col] )
                    |> List.choose (fun (row,col) -> numFromPoint (row,col) table)                    
                    |> List.distinct                    
    ] |> List.distinct |> List.map (snd>>Convert.ToInt32)

File.ReadAllLines("./puzzle.input")
|> Array.take 1
|> fun x -> String.Join('\n',x)
|> parse
|> components
// |> List.sum
|> printfn "%A"
