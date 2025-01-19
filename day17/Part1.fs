namespace Day17

module Part1 =
    // #r "nuget: FSharp.Collections.Builders"

    // open FSharp.Collections.Builders
    open System
    open System.IO
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open Common
    // #load "../global.fsx"


    let parse path =
        path
        |> File.ReadAllLines
        |> Array.map (fun row ->
            row.ToCharArray()
            |> Array.map (fun c -> int (c - '0')))
        |> array2D

    let printVisited (matrix: Matrix) =
        let rows, cols = Matrix.size matrix

        for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                printf "%d " matrix.[row, col]

            printfn ""

    let arrow (row1, col1) (row2, col2) =
        match row2 - row1, col2 - col1 with
        | rowDiff, colDiff when rowDiff = 0 && colDiff > 0 -> '►'
        | rowDiff, colDiff when rowDiff = 0 && colDiff < 0 -> '◄'
        | rowDiff, colDiff when rowDiff > 0 && colDiff = 0 -> '▼'
        | rowDiff, colDiff when rowDiff < 0 && colDiff = 0 -> '▲'
        | x -> failwithf "%A -> %A = %A bad diff" x (row1, col1) (row2, col2)

    let neighborhood (cell: Cell) =
        let row, col = cell

        [ row + 1, col
          row - 1, col
          row, col + 1
          row, col - 1 ]

    let DykeBuilder (matrix: Matrix) = 
        let rows, cols = matrix |> Matrix.size
        let dirs = 4

        let isInBounds row col =
            (Matrix.isOutOfBounds (rows, cols) (row, col))
            |> not

        let initial = 0,0
        let final = rows-1, cols-1
        let visited = 
            (*  Consecutive count is 4 due to starting from zero, so no need to -1 
                the actual number everytime the cache is updated *)
            let v = Array4D.create rows cols dirs 4 Int32.MaxValue
            v.[0,0,Lt.AsInt,0] <- 0
            v
        let toVisit = 
            let q = PriorityQueue<State, int>()
            q.Enqueue(State.From(fst initial, snd initial,Lt,0), 0)
            q

        let rec search (state : State) = 
            if  state.Current = final 
            then visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
            else             
                let n = 
                    neighborhood state.Current 
                    |> List.filter (fun (r,c) -> isInBounds r c)
                
                for row,col in n do
                    let nextDir = Dir.From(state.Current, (row,col))
                    let nextRepeatedDir = 
                        if nextDir = state.Dir 
                        then state.Repeats + 1 else 1

                    let isDoublingBack = nextDir.Opposite() = state.Dir

                    if nextRepeatedDir <= 3 && not isDoublingBack
                    then
                        let lastScore = visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
                        let nextScore = lastScore + matrix.[row,col]    

                        // if row = rows-1 && col = cols-1
                        // then 
                        //     printfn "%A -> %A %d -> %d" state.Current (row,col) lastScore nextScore

                        if nextScore < visited.[row,col,nextDir.AsInt,nextRepeatedDir]
                        then 
                            visited.[row,col,nextDir.AsInt,nextRepeatedDir] <- nextScore
                            toVisit.Enqueue(State.From(row,col,nextDir, nextRepeatedDir), nextScore)

                toVisit.Dequeue() |> search

        toVisit.Dequeue() |> search

        