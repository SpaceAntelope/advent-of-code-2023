open System
open System.Collections.Generic
open System.Text.RegularExpressions

let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> array2D


type Dir = N | E | S | W

type Node = {
    Tile : Char
    Row: int
    Col: int    
}

type Edge = { 
    NodeSource : Node
    NodeDestination: Node
    Direction : Dir*Dir
}

let opposite  = 
    [ W,E
      E,W
      N,S
      S,N ] |> readOnlyDict

let endpoints =
    [|
        '|',[|N;S|]
        '-',[|E;W|]
        'L',[|N;E|]
        'J',[|N;W|]
        '7',[|S;W|]
        'F',[|S;E|]
        'S',[||]
    |] |> readOnlyDict

let positionToString currentRow currentCol (maze: char array2d) =
    let rowLength = maze |> Array2D.length1
    let colLength = maze |> Array2D.length2
    
    let hStart = Math.Max(0,currentCol - 5 )
    let hLength =  Math.Min(10, colLength - hStart)
    let vStart = Math.Max(0,currentRow - 5 )
    let vLength = Math.Min(10, colLength - vStart)

    maze 
    |> Array2D.mapi (fun row col value ->  if row = currentRow && col = currentCol then $"[{value}]" else $" {value} " )
    |> Seq.cast<string> 
    |> Seq.chunkBySize rowLength
    |> Seq.map (fun row -> String.Join(' ', row[hStart .. hStart+hLength]))
    |> Seq.skip vStart
    |> Seq.take vLength
    |> Seq.mapi (fun i line -> sprintf $"{i + vStart} {line}")
    |> Seq.append (
        [hStart..hStart+hLength] 
        |> Seq.map string 
        |> Seq.reduce (fun state current -> $"{state}{current.PadLeft(4,' ')}")
        |> fun x -> $"    {x}" |> Seq.singleton )
    |> Seq.reduce (sprintf "%s\n%s")
    

let direction (src: Node) (dst: Node) =
    match (src.Row, src.Col, dst.Row, dst.Col) with
    | r1,c1,r2,c2 when r1 - r2 = 0 && c1 > c2 -> E,W
    | r1,c1,r2,c2 when r1 - r2 = 0 && c1 < c2 -> W,E
    | r1,c1,r2,c2 when c1 - c2 = 0 && r1 > r2 -> S,N
    | r1,c1,r2,c2 when c1 - c2 = 0 && r1 < r2 -> N,S
    | r1,c1,r2,c2 -> failwith $"Invalid direction: ({r1},{c1}) -> ({r2}, {c2})"
        
let isValidConnection (src: Node) (dst: Node) =
    
    let (dir1,dir2) = direction src dst
    let mutable endpoints1 = endpoints.[src.Tile]
    let mutable endpoints2 = endpoints.[dst.Tile]

    if endpoints1 = [||] then endpoints1 <- endpoints2 |> Array.map (fun x -> opposite.[x])
    if endpoints2 = [||] then endpoints2 <- endpoints1 |> Array.map (fun x -> opposite.[x])

    // printfn "%A %b %b" (dir1,dir2,endpoints1,endpoints2) (endpoints1 |> Array.contains dir2) (endpoints2 |> Array.contains dir1)
    (endpoints1 |> Array.contains dir2) && (endpoints2 |> Array.contains dir1)

let neighbors (source: Node) (maze: char array2d) = 
    let rowLength = maze |> Array2D.length1
    let colLength = maze |> Array2D.length2
    let row = source.Row
    let col = source.Col
    [|row-1,col;row,col-1;(*(row,col);*)row,col+1;row+1,col|] 
    |> Array.filter (fun (r,c) -> 
        r>= 0 && c >= 0 
        && r < rowLength && c < colLength
        && maze[r,c] <> '.') 
    |> Array.map (fun (row, col) -> { Tile = maze.[row,col]; Row = row; Col = col } )


let findStart (maze: char array2d) = 
    let mutable row = 0
    let mutable col = 0
    let rowLength = maze |> Array2D.length1
    let colLength = maze |> Array2D.length2
    while (maze.[row,col]<>'S' && row <= rowLength) do
        col <- col + 1
        if col = colLength then 
            col <- 0
            row <- row + 1
    
    row,col

[<TailCall>]
let rec travel (path: Node ResizeArray) (maze: char array2d) =
    let currentNode = path[path.Count-1]
    
    // positionToString currentNode.Row currentNode.Col maze
    // |> printfn "%s"
    
    let previousNode = if path.Count > 1 then Some path[path.Count-2] else None

    let isNotPreviousNode (n:Node) =
        match previousNode with
        | Some prev -> 
            // printfn "isPrev %b %b " (prev.Col <> n.Col) (prev.Row <> n.Row)
            prev.Col <> n.Col || prev.Row <> n.Row
        | None -> true
        
    let nextNode = 
        neighbors currentNode maze 
        // |> fun x -> printfn "neighborhood %A" x; x
        |> Array.find (fun nextNode -> 
            isNotPreviousNode nextNode
            && isValidConnection currentNode nextNode)
    
    path.Add nextNode
    
    if nextNode.Tile <> 'S' 
    then travel path maze
    
    
let findFurthestDistance (maze: char array2d) =
    let path = ResizeArray()

    findStart maze 
    |> fun (r,c) -> { Tile = maze.[r,c]; Row = r; Col = c }
    |> path.Add

    travel path maze
    
    path 
    |> Seq.mapi (fun i node -> i, path.Count - i - 1) 
    // |> fun x -> printfn "%A" ( Array.ofSeq x); x
    |> Seq.find (fun (x,y) -> x = y)
    |> fst
    

let inline printDir (d: IReadOnlyDictionary<_, _>) =
    d.Keys
    |> Seq.iter(fun key -> printfn "%A: %A" key d[key])

let printPosition curretnRow currentCol (maze: char array2d) =
    maze 
    |> Array2D.mapi (fun row col value ->  if row = curretnRow && col = currentCol then '*' else value )
    |> sprintf "%A\n"
    |> fun x -> Regex.Replace(x, "[;\'\[\]]","")
   // |> printfn "%A"


let toNode row col (maze: char array2d) = { Tile = maze.[row,col]; Row = row; Col = col }