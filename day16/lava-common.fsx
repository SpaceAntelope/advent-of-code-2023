open System
open System.Collections.Generic


type Tile = SplitV | SplitH | LeanRight | LeanLeft | Empty
type Point = { Row: int; Col: int; Val: Tile }
type Direction = ToBottom | ToRight | ToTop | ToLeft

let decodeTile (c: char) =
    match c with
    | '|' -> SplitV
    | '-' -> SplitH
    | '/' -> LeanRight
    | '\\' -> LeanLeft
    | '.' -> Empty
    | x -> failwith $"I don't know how to %A{x}."

let encodeTile (t: Tile) = 
    match t with 
    | SplitV -> '|'
    | SplitH -> '-'
    | LeanRight -> '/'
    | LeanLeft -> '\\'
    | Empty -> '.'

let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> array2D    
    |> Array2D.mapi (fun row col value -> { Row = row; Col = col; Val = decodeTile value } )
 
let tripTheLight (startDir:Direction) (start: int*int) (table : Point array2d)  = 
    let rowStart, colStart = start
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    let visited = Array2D.init rowCount colCount (fun row col -> encodeTile table[row,col].Val)
    let followed = HashSet<Point*Direction>()

    let rec fantastick (dir: Direction) (current: Point) =
        let msg = $"{current.Row} {current.Col} {current.Val} while going %A{dir}"
        let onStop() = ()//printfn $"*STOP* {msg}"
        visited[current.Row,current.Col] <- '#'
        if followed.Add(current,dir) 
        then 
            match current.Val, dir with
            (* beam stops *)
            | SplitV, ToBottom when current.Row = rowCount - 1 -> onStop()
            | SplitV, ToTop when current.Row = 0 -> onStop()
            | SplitH, ToLeft when current.Col = 0 -> onStop()
            | SplitH, ToRight when current.Col = colCount - 1 -> onStop()
            | LeanRight, ToRight when current.Row = 0 -> onStop()
            | LeanRight, ToLeft when current.Row = rowCount-1 -> onStop()
            | LeanRight, ToBottom when current.Col = 0 -> onStop()
            | LeanRight, ToTop when current.Col = colCount-1 -> onStop()
            | LeanLeft, ToRight when current.Row = rowCount-1 -> onStop()
            | LeanLeft, ToLeft when current.Row = 0 -> onStop()
            | LeanLeft, ToBottom when current.Col = rowCount - 1  -> onStop()
            | LeanLeft, ToTop when current.Col = 0 -> onStop()
            | Empty, ToTop when current.Row = 0 -> onStop()
            | Empty, ToBottom when current.Row = rowCount-1 -> onStop()
            | Empty, ToLeft when current.Col = 0 -> onStop()
            | Empty, ToRight when current.Col = colCount-1 -> onStop()
            (* beam coninues *)
            | SplitV, ToBottom
            | Empty, ToBottom -> fantastick ToBottom table[current.Row + 1, current.Col]
            | SplitV, ToTop
            | Empty, ToTop -> fantastick ToTop table[current.Row - 1, current.Col]
            | SplitV, ToLeft 
            | SplitV, ToRight -> 
                if current.Row < rowCount-1
                then fantastick ToBottom table[current.Row + 1, current.Col]
                else onStop()
                if current.Row > 0
                then fantastick ToTop table[current.Row - 1, current.Col]
                else onStop()
            | SplitH, ToLeft 
            | Empty, ToLeft -> fantastick ToLeft table[current.Row, current.Col-1]
            | SplitH, ToRight 
            | Empty, ToRight -> fantastick ToRight table[current.Row, current.Col+1]
            | SplitH, ToBottom
            | SplitH, ToTop ->
                if current.Col < colCount-1
                then fantastick ToRight table[current.Row, current.Col+1]
                else onStop()
                if current.Col > 0
                then fantastick ToLeft table[current.Row, current.Col-1]
                else onStop()
            | LeanRight, ToRight -> fantastick ToTop table[current.Row-1, current.Col]
            | LeanRight, ToLeft -> fantastick ToBottom table[current.Row+1, current.Col]
            | LeanRight, ToBottom -> fantastick ToLeft table[current.Row, current.Col-1]
            | LeanRight, ToTop -> fantastick ToRight table[current.Row, current.Col+1]
            | LeanLeft, ToRight -> fantastick ToBottom table[current.Row+1, current.Col]
            | LeanLeft, ToLeft -> fantastick ToTop table[current.Row-1, current.Col]
            | LeanLeft, ToBottom -> fantastick ToRight table[current.Row, current.Col+1]
            | LeanLeft, ToTop -> fantastick ToLeft table[current.Row, current.Col-1]
            

    fantastick startDir table[rowStart,colStart]

    visited

let countEnergized (table: char array2d) =
    table 
    |> 
    Seq.cast<char> 
    |> Seq.filter (fun c -> c = '#')
    |> Seq.length

let startingVectors (table: _ array2d) =
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    let rows = [|0..rowCount-1|]
    let cols = [|0..colCount-1|]
    Array.allPairs rows cols
    |> Array.filter (fun (row,col) -> row = 0 || col = 0 || row = rowCount - 1 || col = colCount - 1)
    |> Array.collect(fun (row,col) ->
        match row,col with
        | r,c when r = 0 && c = 0 ->
            [| ToRight, (r,c); ToBottom, (r,c)|]
        | r,c when r = 0 && c = colCount-1 ->
            [| ToLeft, (r,c); ToBottom, (r,c)|]
        | r,c when r = rowCount-1 && c = 0 ->
            [| ToRight, (r,c); ToTop, (r,c)|]
        | r,c when r = rowCount-1 && c = colCount-1 ->
            [| ToLeft, (r,c); ToTop, (r,c)|]
        | r,c when r = 0 -> [|ToBottom, (r,c)|]
        | r,c when r = rowCount-1 -> [|ToTop, (r,c)|]
        | r,c when c = 0 -> [|ToRight, (r,c)|]
        | r,c when c = colCount-1 -> [|ToLeft, (r,c)|]
        | stuff -> failwithf "Wait what %A" stuff
    )

    