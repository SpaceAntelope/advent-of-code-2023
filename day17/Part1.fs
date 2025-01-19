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
        member x.Opposite() =
            match x with
            | Rt ->  Lt
            | Lt ->  Rt
            | Dn ->  Up
            | Up ->  Dn


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

    type State = {
        Row: int
        Col: int 
        Dir: Dir
        Repeats: int
    } 
    with 
        member x.Current = (x.Row,x.Col)
        static member From(row,col, dir,repeats) = { Row=row;Col=col;Dir=dir;Repeats=repeats }

    let DykeBuilder (matrix: Matrix) = 
        let rows, cols = matrix |> Matrix.size
        let dirs = 4
        // consecutive is 4 due to starting from zero
        let path = 
            matrix 
            |> Matrix.allIndices 
            |> Seq.fold (fun (state: Dictionary<Cell,Cell*Dir>) current -> 
                state.Add(current, ((-1,-1),Lt)); state ) (Dictionary<Cell,Cell*Dir>())

        let isInBounds row col =
            (Matrix.isOutOfBounds (rows, cols) (row, col))
            |> not

        let initial = 0,0
        let finale = rows-1, cols-1
        let mutable state = State.From(-1,-1, Lt, 0)
        let visited = Array4D.create rows cols dirs 4 Int32.MaxValue
        visited.[0,0,Lt.AsInt,0] <- 0
        let toVisit = 
            let q = PriorityQueue<State, int>()
            q.Enqueue(State.From(fst initial, snd initial,Lt,0), 0)
            q

        let mutable sentinel = true

        while sentinel do
            state <- toVisit.Dequeue()
            
            sentinel <- state.Current <> finale

            if sentinel 
            then 
                let n = 
                    neighborhood state.Current 
                    |> List.filter (fun (r,c) -> isInBounds r c)
                
                for row,col in n do
                    // printfn "%A %A" state (row,col)
                    let nextDir = Dir.From(state.Current, (row,col))
                    let nextRepeatedDir = 
                        if nextDir = state.Dir 
                        then state.Repeats + 1 else 1

                    let isDoublingBack = nextDir.Opposite() = state.Dir

                    if nextRepeatedDir <= 3 && not isDoublingBack
                    then
                        let lastScore = visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]
                        let nextScore = lastScore + matrix.[row,col]    

                        if row = 12 && col = 12 
                        then 
                            printfn "%A -> %A %d -> %d" state.Current (row,col) lastScore nextScore

                        if nextScore < visited.[row,col,nextDir.AsInt,nextRepeatedDir]
                        then 
                            visited.[row,col,nextDir.AsInt,nextRepeatedDir] <- nextScore
                            path.[(row,col)] <- state.Current, nextDir
                            toVisit.Enqueue(State.From(row,col,nextDir, nextRepeatedDir), nextScore)

            
        let score = visited.[state.Row,state.Col,state.Dir.AsInt,state.Repeats]


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

        score


        // let finalScore = 
        //     (path,(0,0))
        //     |> Seq.unfold (fun (state: Dictionary<Cell,Cell>*Cell) ->  
        //             let dic, pos = state
        //             let parent = dic.[pos]
        //             if parent = (0,0)
        //             then None
        //             else Some ((dic,parent), matrix.[fst parent, snd parent]))
        //     |> Seq.sum
        
        // finalScore

    let pathFinding (matrix: Matrix) =

        let rows, cols = matrix |> Matrix.size
        // let visited = Array2D.create rows cols (Int32.MaxValue,Lt,0)
        let mutable visited  = Map.empty<int*int*Dir*int, int>
        let isVisitable row col dir times score = 
            visited 
            |> Map.tryFind (row,col,dir,times)
            |> function 
            | Some oldScore when oldScore > score -> 
                    //visited <- visited |> Map.change (row,col,dir,times) (fun _ -> Some score)
                    true
            | Some _ -> false                
            | None -> true

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
            // (vector: Cell * Cell * Cell)
            (bias: Dir*int)
            // (movesSinceLastTurn: int)
            (score: int)
            (path: char array2d)
            (depth: int)
            =
            let r, c = current
            let lastDir,times = bias
            let visitedKey = r,c,lastDir,times
            match visited |> Map.tryFind visitedKey with
            | Some x -> 
                    visited <- visited |> Map.change visitedKey (fun _ -> Some score)
            | None -> 
                    visited <- visited |> Map.add visitedKey score

            // visited.[r, c] <- score, dir, times

            // printfn "%03d:%s%A" (depth) ("".PadLeft(depth)) current

            // if x then
            //     Matrix.printBase path []
            //     printfn "HO HO HO %d %A -> %A" score current (neighborhood current)

            //     neighborhood current
            //     |> List.filter (fun (r, c) -> isInBounds r c)
            //     |> List.iter (fun (r, c) -> 
            //         printfn $"%A{(r, c)}: {score + matrix.[r, c]} vs {visited.[r, c]}")

            // printVisited visited
            // Console.ReadLine() |> ignore
            //if current = start then
                // printfn "HEY HEY HEY %A" (neighborhood current)

            if current = (0, 1) && score = matrix.[0, 1] then
                printfn "HO HO HO %d %A -> %A" score current (neighborhood current)
                //printVisited visited
                x <- true
                // Matrix.printBase path []

            if current = stop then
                printfn "Score: %i" score
                // printVisited visited
                Matrix.printBase path []
                [ score ]
            else
                // let (r1, c1), (r2, c2), (r3, c3) = vector

                current
                |> neighborhood
                |> List.filter (fun (row, col) ->
                    isInBounds row col 
                    //&& isVisitable row col lastDir times (score + matrix.[row, col])
                    // && visited.[row, col, dir, times] > (score + matrix.[row, col])
                // && visited.[row,col] = Int32.MaxValue
                )
                // |> General.teeConditional (fun () -> current = (0, 1)) $"Current %A{current}"
                // |> List.sortBy (fun (row, col) -> matrix.[row, col])
                |> List.collect (fun (row, col) ->
                    // visited.[row,col] <- score + matrix.[row,col]

                    let newPath = Array2D.copy path
                    newPath.[row, col] <- arrow current (row, col) //'0' + char matrix.[row,col]
                    
                    let newDir, newTimes = 
                        let newDir = Dir.From(current, (row,col))
                        if lastDir = newDir 
                        then newDir, times + 1
                        else newDir, 1  

                    let isMovingToPreviousStep = 
                        let newDir = Dir.From(current, (row,col))
                        match newDir,lastDir with 
                        | Lt, Rt
                        | Rt, Lt
                        | Up, Dn
                        | Dn, Up -> true
                        | _ -> false

                    let isUnvisitedOrHigherScored = 
                        isVisitable row col newDir newTimes (score + matrix.[row, col])

                    if newTimes > 3 || isMovingToPreviousStep || not isUnvisitedOrHigherScored then
                        []
                    else
                        search
                            (row, col)
                            (newDir, newTimes)
                            (score + matrix.[row, col])
                            newPath
                            (depth + 1))

        search start (Lt,0) 0 path 0
