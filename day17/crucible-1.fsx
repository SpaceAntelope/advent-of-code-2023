open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

#load "../global.fsx"

type Matrix = char array2d
type Cell = int*int
type Dir = Fwd | Lt | Rt 
    // with 
    //     member x.Next(row,col) = 
    //         match x with
    //         | Lt -> 
let parse path = 
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

let neighborhood (cell: Cell) =
    let row,col = cell
    [row+1,col;row-1,col;row,col+1;row,col-1]

let pathFinding (matrix: Matrix) =
    let visited = 
        matrix 
        |> Global.matrixIndices 
        |> Seq.map (fun cell -> cell,Int32.MaxValue) 
        |> dict        
    let rows,cols = matrix |> Global.matrixSize
    let isInBounds = (Global.isOutOfBounds (rows,cols))>>not
    let start = 0,0
    let stop = rows-1,cols-1
    
    let rec search (current: Cell) (vector: Cell*Cell*Cell) (score:int) =
        visited.[current] <- score
        if current = stop 
        then score
        else
            let (r1,c1),(r2,c2),(r3,c3) = vector
            current
            |> neighborhood         
            |> List.filter (fun (row,col) -> 
                isInBounds (row,col) 
                && row,col <> r3,c3 
                && visited.[row,col] > score + matrix.[row,col]
                && not (r1 = r2 && r2 = r3 && r3 = row) 
                && not (c1 = c2 && c2 = c3 && c3 = col) )
            |> List.collect(fun (row, col) -> search (row,col) ((r2,c2),(r3,c3),(row,col)) (score + matrix.[row,col]) )
