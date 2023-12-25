open System
open System.Collections.Generic

let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> array2D

[<TailCall()>]
let rec indexOfObstacle (line: char[]) (index : int ) =
    if line[index] <> '.' || index = 0
    then index 
    else indexOfObstacle line (index-1)

[<TailCall()>]
let rec indexOfMovable (line: char[]) (index : int ) =
    if index = line.Length - 1 || line[index] <> '.' 
    then (index,line[index])
    else indexOfMovable line (index+1)

let tiltNorth (table: char array2d) =   
    let pointNowDot = HashSet<int*int>()
    let pointNowRock =HashSet<int*int>()
    let updateLine (line : char[])  colIndex =
        line |> Array.mapi (fun i value -> 
            if pointNowDot.Contains(i,colIndex)
            then '.'
            else if pointNowRock.Contains(i,colIndex)
            then 'O'
            else value)

    table
    |> Array2D.mapi (fun row col value -> 
        let updatedValue = 
            if pointNowDot.Contains(row,col) then '.'
            else if pointNowRock.Contains(row,col) then 'O'
            else value

        if updatedValue = '.'
        then 
            let updatedRow = updateLine table[*,col] col
            let boundaryIndex,valueBelow = indexOfMovable updatedRow row
            if valueBelow = 'O'
            then 
                pointNowDot.Add((boundaryIndex,col)) |> ignore
                pointNowRock.Add(row,col) |> ignore
                'O'
            else updatedValue
        else updatedValue)    

    // for row in 1..rowCount-1 do
    //     for col in 0..colCount-1 do
    //         if table[row,col] = 'O' 
    //         then 
    //             let boundaryIndex = indexOfObstacle table[row,0..col] row
    //             if boundaryIndex < row - 1 then 
    //                 table[boundaryIndex, col] <- 'O'
    //                 table[row,col] <- '.'

type Dir = N | E | S | W

let tilt (direction: Dir) (table: char array2d) =
    let pointNowDot = HashSet<int*int>()
    let pointNowRock =HashSet<int*int>()
    let updateLine (line : char[])  colIndex =
        line |> Array.mapi (fun i value -> 
            if pointNowDot.Contains(i,colIndex)
            then '.'
            else if pointNowRock.Contains(i,colIndex)
            then 'O'
            else value)

    table
    |> Array2D.mapi (fun row col value -> 
        let updatedValue = 
            if pointNowDot.Contains(row,col) then '.'
            else if pointNowRock.Contains(row,col) then 'O'
            else value

        if updatedValue = '.'
        then 
            let updatedRow = 
                match direction with
                | N -> updateLine table[*,col] col
                | S -> updateLine table[*,col] col |> Array.rev
                | E -> updateLine table[row,*] row
                | W -> updateLine table[row,*] row |> Array.rev
                
            let boundaryIndex,boundaryValue = indexOfMovable updatedRow row

            if boundaryValue = 'O'
            then 
                pointNowDot.Add((boundaryIndex,col)) |> ignore
                pointNowRock.Add(row,col) |> ignore
                'O'
            else updatedValue
        else updatedValue)    

let score (table : char array2d) =
    let rowCount = table |> Array2D.length1

    [0..rowCount-1]
    |> List.mapi (fun i rowIndex -> (rowCount - i), table.[rowIndex,*] )
    |> List.map (
        fun (weight, row) -> 
            row 
            |> Array.fold(fun state current -> if current = 'O' then state + weight else state) 0)
    |> List.sum
