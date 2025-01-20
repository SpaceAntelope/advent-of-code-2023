namespace Day17

module Part2 =

    open System
    open System.Collections.Generic
    open Common

    (*
        Neighbor generation and next state creation should probably be merged
        to prevent some logic duplication.
    *)

    let DykeBuilder (matrix: Matrix) = 
        let rows, cols = matrix |> Matrix.size
        let dirs = 4
        let minConsecutiveSameDirMoves, maxConsecutiveSameDirMoves = 4,10

        let isInBounds row col =
            (Matrix.isOutOfBounds (rows, cols) (row, col))
            |> not

        let initial = 0,0
        let final = rows-1, cols-1
        let visited = 
            (*  Consecutive count is 4 due to starting from zero, so no need to -1 
                the actual number everytime the cache is updated *)
            let v = Array4D.create rows cols dirs (maxConsecutiveSameDirMoves+1) Int32.MaxValue
            v.[0,0,Lt.AsInt,0] <- 0
            v
        let toVisit = 
            let q = PriorityQueue<State, int>()
            q.Enqueue(State.From(fst initial, snd initial,Lt,0), 0)
            q

        let path = Dictionary<State,State>()

        let neighborhood (state: State) =
            match state.Repeats with
            | 0 -> 
                [Up;Dn;Lt;Rt] 
                |> List.map(fun dir -> dir.Next(state.Current))
            | x when x < minConsecutiveSameDirMoves -> 
                [state.Dir.Next(state.Current)] 
            | x when x <= maxConsecutiveSameDirMoves -> 
                [Up;Dn;Lt;Rt] 
                |> List.filter (fun dir -> dir <> state.Dir.Opposite)
                |> List.map(fun dir -> dir.Next(state.Current))
            | _ -> []
            |> List.filter (fun (r,c) -> isInBounds r c)

        let rec search (state : State) = 
            if state.Current = final then printfn "%A %d" state visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]

            if  state.Current = final
            then
                printfn "Final state: %A"  state
                visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
            else             
                let n = neighborhood state 
                
                for row,col in n do
                    let nextDir = Dir.From(state.Current, (row,col))
                    let nextRepeatedDir = 
                        if nextDir = state.Dir 
                        then state.Repeats + 1 
                        else 1
                    
                    let lastScore = visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
                    let nextScore = lastScore + matrix.[row,col]    

                    if  nextRepeatedDir <= 10
                        && nextScore < visited.[row,col,nextDir.AsInt,nextRepeatedDir] 
                        && ((row,col) <> final || (row,col)=final && nextRepeatedDir >= 4)
                    then
                        let nextState = State.From(row,col,nextDir, nextRepeatedDir)
                        visited.[row,col,nextDir.AsInt,nextRepeatedDir] <- nextScore
                        toVisit.Enqueue(nextState, nextScore)
                        path.Add(nextState, state)

                toVisit.Dequeue() |> search

        let finalScore = toVisit.Dequeue() |> search
            

        path.Keys 
        |> Seq.filter (fun state -> state.Current = final) 
        |> Seq.map (fun finalState -> 
            let mutable parent = path.[finalState]
            [
                yield finalState
                yield parent
                while parent.Current <> initial do
                    parent <- path.[parent]
                    yield parent
            ])
        |> Seq.iter (fun cellsOfPath -> 
            let pathMatrix = 
                Array2D.init rows cols (fun row col -> 
                    match cellsOfPath |> List.tryFind (fun state -> state.Current = (row,col)) with
                    | Some x -> x.Dir.ToArrow()
                    | None -> '0' + char matrix.[row,col])
            
            Matrix.printBase pathMatrix []
            
            cellsOfPath
            |> Seq.head
            |> fun state -> 
                printfn "Score %d" visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]

            cellsOfPath
            |> Seq.map _.Repeats
            |> fun r -> printfn "Reps: Max: %d Min: %d" (Seq.max r) (Seq.min r)
        )

        finalScore
