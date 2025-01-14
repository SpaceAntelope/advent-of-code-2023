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

    type Matrix = int array2d
    type Cell = int * int

    type Dir =
        | Fwd
        | Lt
        | Rt
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

    let pathFinding (matrix: Matrix) =

        let rows, cols = matrix |> Matrix.size
        let visited = Array2D.create rows cols Int32.MaxValue

        let path =
            Array2D.init rows cols (fun row col -> '0' + char matrix.[row, col])

        let isInBounds row col =
            (Matrix.isOutOfBounds (rows, cols) (row, col))
            |> not

        let start = 0, 0
        let stop = rows - 1, cols - 1
        let mutable x = false

        let rec search
            (current: Cell)
            (vector: Cell * Cell * Cell)
            (movesSinceLastTurn: int)
            (score: int)
            (path: char array2d)
            (depth: int)
            =
            let r, c = current
            visited.[r, c] <- score

            // printfn "%03d:%s%A" (depth) ("".PadLeft(depth)) current
            if x then
                Matrix.printBase path []
                printfn "HO HO HO %d %A -> %A" score current (neighborhood current)

                neighborhood current
                |> List.filter (fun (r, c) -> isInBounds r c)
                |> List.iter (fun (r, c) -> 
                    printfn $"%A{(r, c)}: {score + matrix.[r, c]} vs {visited.[r, c]}")
            // printVisited visited
            // Console.ReadLine() |> ignore
            if current = start then
                printfn "HEY HEY HEY %A" (neighborhood current)

            if current = (0, 1) && score = matrix.[0, 1] then
                printfn "HO HO HO %d %A -> %A" score current (neighborhood current)
                //printVisited visited
                x <- true
                Matrix.printBase path []

            if current = stop then
                printfn "Score: %i" score
                // printVisited visited
                // Matrix.printBase path []
                [ score ]
            else
                let (r1, c1), (r2, c2), (r3, c3) = vector

                current
                |> neighborhood
                |> List.filter (fun (row, col) ->
                    isInBounds row col
                    && visited.[row, col] > (score + matrix.[row, col])
                // && visited.[row,col] = Int32.MaxValue
                )
                |> General.teeConditional (fun () -> current = (0, 1)) $"Current %A{current}"
                |> List.sortBy (fun (row, col) -> matrix.[row, col])
                |> List.collect (fun (row, col) ->
                    // visited.[row,col] <- score + matrix.[row,col]

                    let newPath = Array2D.copy path
                    newPath.[row, col] <- arrow current (row, col) //'0' + char matrix.[row,col]

                    let updatedMovesSinceLastTurn =
                        if r2 <> row && c2 <> col then
                            1
                        else
                            movesSinceLastTurn + 1

                    if updatedMovesSinceLastTurn >= 3 then
                        []
                    else
                        search
                            (row, col)
                            ((r2, c2), (r3, c3), (current))
                            updatedMovesSinceLastTurn
                            (score + matrix.[row, col])
                            newPath
                            (depth + 1))

        search start ((-1, -1), (-2, -2), (-3, -3)) 0 0 path 0
