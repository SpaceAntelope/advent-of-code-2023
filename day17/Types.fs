namespace Day17


type Matrix = int array2d
type Cell = int * int

type Dir =
    | Up
    | Dn
    | Lt
    | Rt
with 
    static member From((row1,col1),(row2,col2)) =
        match row2 - row1, col2 - col1 with
        | rowDiff, colDiff when rowDiff = 0 && colDiff > 0 -> Rt
        | rowDiff, colDiff when rowDiff = 0 && colDiff < 0 -> Lt
        | rowDiff, colDiff when rowDiff > 0 && colDiff = 0 -> Dn
        | rowDiff, colDiff when rowDiff < 0 && colDiff = 0 -> Up
        | x -> failwithf "%A -> %A = %A bad diff" x (row1, col1) (row2, col2)
    member x.AsInt = 
        match x with
        | Up -> 0
        | Rt -> 1
        | Dn -> 2
        | Lt -> 3
    member x.ToArrow() =
        match x with
        | Rt ->  '►'
        | Lt ->  '◄'
        | Dn ->  '▼'
        | Up ->  '▲'
    member x.Opposite =
        match x with
        | Rt ->  Lt
        | Lt ->  Rt
        | Dn ->  Up
        | Up ->  Dn
    
    member x.Next(cell: Cell) =
        match cell,x with
        | (row,col), Up -> row-1,col
        | (row,col), Rt -> row,col+1
        | (row,col), Dn -> row+1,col
        | (row,col), Lt -> row,col-1

type State = {
        Row: int
        Col: int 
        Dir: Dir
        Repeats: int
    } 
    with 
        member x.Current = (x.Row,x.Col)
        static member From(row,col, dir,repeats) = { Row=row;Col=col;Dir=dir;Repeats=repeats }

