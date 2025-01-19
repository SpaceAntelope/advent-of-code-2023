namespace Day17

module Part2 =

    open System
    open System.IO
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open Common


    let arrow (row1, col1) (row2, col2) =
        match row2 - row1, col2 - col1 with
        | rowDiff, colDiff when rowDiff = 0 && colDiff > 0 -> '►'
        | rowDiff, colDiff when rowDiff = 0 && colDiff < 0 -> '◄'
        | rowDiff, colDiff when rowDiff > 0 && colDiff = 0 -> '▼'
        | rowDiff, colDiff when rowDiff < 0 && colDiff = 0 -> '▲'
        | x -> failwithf "%A -> %A = %A bad diff" x (row1, col1) (row2, col2)



    let DykeBuilder (matrix: Matrix) = 
        let rows, cols = matrix |> Matrix.size
        let dirs = 4
        let minConsecutiveSameDirMoves, maxConsecutiveSameDirMoves = 4,10
        // consecutive is 4 due to starting from zero
        // let path = 
        //     matrix 
        //     |> Matrix.allIndices 
        //     |> Seq.fold (fun (state: Dictionary<Cell,Cell*Dir>) current -> 
        //         state.Add(current, ((-1,-1),Lt)); state ) (Dictionary<Cell,Cell*Dir>())
        let neighborhood (cell: Cell) =
            let row, col = cell

            [   row + minConsecutiveSameDirMoves, col
                row - minConsecutiveSameDirMoves, col
                row, col + minConsecutiveSameDirMoves
                row, col - minConsecutiveSameDirMoves ]

        let isInBounds row col =
            (Matrix.isOutOfBounds (rows, cols) (row, col))
            |> not

        let initial = 0,0
        let final = rows-1, cols-1
        let visited = 
            let v = Array4D.create rows cols dirs maxConsecutiveSameDirMoves Int32.MaxValue
            v.[0,0,Lt.AsInt,0] <- 0
            v
        let toVisit = 
            let q = PriorityQueue<State, int>()
            q.Enqueue(State.From(fst initial, snd initial,Lt,0), 0)
            q

        let rec search (state : State) = 
            if state.Current = final 
            then visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
            else             
                let n = 
                    neighborhood state.Current 
                    |> List.filter (fun (r,c) -> isInBounds r c)
                
                for row,col in n do
                    let nextDir = Dir.From(state.Current, (row,col))
                    let nextRepeatedDir = 
                        if nextDir = state.Dir 
                        then state.Repeats + 1 else 4

                    let isDoublingBack = nextDir.Opposite() = state.Dir

                    if nextRepeatedDir <= 10 && not isDoublingBack
                    then
                        let lastScore = visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
                        let nextScore = 
                            if nextRepeatedDir = 4
                            then 
                                match nextDir with
                                | Rt -> matrix.[row, col-4..col]
                                | Lt -> matrix.[row, col..col+4]
                                | Up -> matrix.[row-4..row, col]
                                | Dn -> matrix.[row..row+4, col]
                                |> Array.sum 
                                |> (+) lastScore
                            else lastScore + matrix.[row,col]

                        // if row = rows-1 && col = cols-1
                        // then 
                        //     printfn "%A -> %A %d -> %d" state.Current (row,col) lastScore nextScore

                        if nextScore < visited.[row,col,nextDir.AsInt,nextRepeatedDir]
                        then 
                            visited.[row,col,nextDir.AsInt,nextRepeatedDir] <- nextScore
                            toVisit.Enqueue(State.From(row,col,nextDir, nextRepeatedDir), nextScore)

                toVisit.Dequeue() |> search

        toVisit.Dequeue() |> search



        // let cellsOfPath = 
        //     [
        //         let mutable parent = path.[rows-1,cols-1]
        //         yield parent
        //         while fst parent <> (0,0) do
        //             printfn "%A" parent
        //             let (row,col), dir = parent
        //             parent <- path.[row,col]
        //             yield parent
        //     ] 
        //     |> Map.ofList

        // let pathMatrix = Array2D.init rows cols (fun row col -> 
        //     match cellsOfPath |> Map.tryFind (row,col) with
        //     | Some x -> x.ToArrow()
        //     | None -> '0' + char matrix.[row,col])

        // Matrix.printBase pathMatrix []

