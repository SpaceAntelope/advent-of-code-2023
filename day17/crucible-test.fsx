open System
open System.Collections.Generic
open System.Text.RegularExpressions

let testData = @"
    2413432311323
    3215453535623
    3255245654254
    3446585845452
    4546657867536
    1438598798454
    4457876987766
    3637877979653
    4654967986887
    4564679986453
    1224686865563
    2546548887735
    4322674655533"
    
    
let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> Array.map (Array.map( fun c -> int c - 48))
    |> array2D   

parse testData |> printfn "%A"

type Direction = Top | Bottom | Left | Right
type Point = { Row: int; Col: int }

let expand (dir: Direction) (p: Point) =
    [|
        if dir <> Top then {p with Row = p.Row + 1}, Bottom
        if dir <> Bottom then {p with Row = p.Row - 1}, Top
        if dir <> Left then {p with Col = p.Col + 1}, Right
        if dir <> Right then {p with Col = p.Col - 1}, Left  
    |]


let printPath (path: Stack<Direction*Point>) (table: int array2d) =
    let index = 
        path 
        |> Seq.map(fun (dir,p) -> (p.Row,p.Col),dir) 
        |> readOnlyDict
    
    table
    |> Array2D.mapi (fun row col value -> 
        if index.ContainsKey(row,col) 
        then 
            match index[row,col] with 
            | Top -> $"{value}^"
            | Bottom -> $"{value}v"
            | Left ->  $"{value}<"
            | Right ->  $"{value}>"
        else $"{value} ")
    |> sprintf "%A"
    |> fun str -> Regex.Replace(str,"[\\[\"\\];]|^\\s*","", RegexOptions.Multiline)
    |> printfn "\n%s\n\n"
    
let travel (start: Direction*int*int) (table: int array2d) =
    let startDir,startRow, startCol = start;
    //let visited  = Hashset
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    let noCycle = HashSet<Direction*Point>()
    let history = ResizeArray<Point>()
    let heatLoss = ResizeArray<int>() 
    let path = Stack<Direction*Point>()
    // [<TailCall>]
    path.Push(Bottom,{Row=startRow;Col=startCol})

    
    let rec search (sameDirectionCounter: int) (sum:int) (dir: Direction) (p: Point)  =
        if p.Row = rowCount-1 && p.Col = colCount - 1
        then
            printPath path table
            printf "Total heat loss: %d" sum 
            heatLoss.Add(sum)
        else if (noCycle.Add(dir,p))
        then
            let nextPoints = 
                expand dir p
                |> Array.filter (fun (nextp,_) -> nextp.Row >= 0 && nextp.Col >= 0 && nextp.Row < rowCount && nextp.Col < colCount )
                |> Array.filter (fun (_,nextd) -> sameDirectionCounter < 3 || nextd <> dir)
                |> Array.sortBy (fun (nextp, _) -> table[nextp.Row, nextp.Col], if dir = Bottom || dir = Right then 1 else 2)
            
            for (nextPoint,nextDirection) in nextPoints do
                path.Push(nextDirection,nextPoint)
                search (if nextDirection = dir then sameDirectionCounter + 1 else 0) (sum + table[nextPoint.Row, nextPoint.Col]) nextDirection nextPoint
                path.Pop() |> ignore
    
    search 0 0 startDir {Row=startRow;Col=startCol}

    heatLoss

testData
|> parse
|> travel (Bottom,0,0) 
|> printfn "%A"