// #r "nuget: FSharp.Collections.Builders"

// open FSharp.Collections.Builders
open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

#load "../global.fsx"

type Matrix = int array2d
type Cell = int*int
type Dir = Fwd | Lt | Rt 
    // with 
    //     member x.Next(row,col) = 
    //         match x with
    //         | Lt -> 
let parse path = 
    path
    |> File.ReadAllLines
    |> Array.map (fun row -> 
        row.ToCharArray() 
        |> Array.map (fun c -> int (c - '0')))
    |> array2D

let printVisited (matrix: Matrix) =
    let rows,cols= Global.matrixSize matrix

    for row in 0..rows-1 do
        for col in 0..cols-1 do
            printf "%d " matrix.[row,col]
        printfn ""


let neighborhood (cell: Cell) =
    let row,col = cell
    [row+1,col;row-1,col;row,col+1;row,col-1]

let pathFinding (matrix: Matrix) =
    
    let rows,cols = matrix |> Global.matrixSize
    let visited = Array2D.create rows cols Int32.MaxValue
    let path = Array2D.create rows cols '.'
    let isInBounds row col = (Global.isOutOfBounds (rows,cols) (row,col)) |> not
    let start = 0,0
    let stop = rows-1,cols-1
    
    let rec search (current: Cell) (vector: Cell*Cell*Cell) (movesSinceLastTurn: int) (score:int) (path: char array2d) =
        visited.[fst current, snd current] <- score

        // Global.printMatrixBase path []
        // printVisited visited
        // Console.ReadLine() |> ignore
        if current = start 
        then printfn "HEY HEY HEY"

        if current = stop 
        then 
            printfn "Score: %i" score
            Global.printMatrixBase path []
            printVisited visited
            [score]
        else
            let (r1,c1),(r2,c2),(r3,c3) = vector
            current
            |> neighborhood         
            |> List.filter (fun (row,col) -> 
                isInBounds row col
                // && (row,col) <> (r3,c3) 
                && visited.[row,col] > (score + matrix.[row,col])
                && movesSinceLastTurn <= 3
                // && not (r1 = r2 && r2 = r3 && r3 = row) 
                // && not (c1 = c2 && c2 = c3 && c3 = col) 
                )
            |> List.sortBy (fun (row,col) -> matrix.[row,col])
            |> List.collect(fun (row, col) -> 
                    // visited.[row,col] <- score + matrix.[row,col]

                    let newPath = Array2D.copy path
                    newPath.[row,col] <- '0' + char matrix.[row,col]
                    
                    let updatedMovesSinceLastTurn =
                        if r2 <> row && c2 <> col 
                        then 1
                        else movesSinceLastTurn + 1

                    search (row,col) ((r2,c2),(r3,c3),(row,col)) updatedMovesSinceLastTurn (score + matrix.[row,col]) newPath)

    search start ((-1,-1),(-2,-2),(-3,-3)) 0 0 path

"./puzzle.example"
|> parse
|> pathFinding
|> List.min
|> Global.shouldBe 102