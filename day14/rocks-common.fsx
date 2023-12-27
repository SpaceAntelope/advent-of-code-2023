open System
open System.Collections.Generic

type Dir = N | E | S | W

let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> array2D

[<TailCall()>]
let rec indexOfObstacle (line: char[]) (index : int ) (dir: Dir) =
    // if line[index] <> '.' 
    // then index, line[index] 
    // else 
        match dir with
        | N | W when index = 0 -> index, line[index]  
        | N | W when index > 0 && line[index-1] <> '.' -> index-1, line[index-1] 
        | N | W -> indexOfObstacle line (index-1) dir
        | S | E when index = line.Length - 1 -> index, line[index]  
        | S | E when index < line.Length - 1 && line[index+1] <> '.' -> index+1, line[index+1]  
        | S | E -> indexOfObstacle line (index+1) dir

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

let tiltInPlace  (direction: Dir) (table: char array2d) = 

    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2

    match direction with
    | N -> 
        for row in 0..rowCount-1 do
            for col in 0..colCount-1 do
                if table[row,col] = 'O' 
                then 
                    let obstacleIndex, obstacleItem = indexOfObstacle table[*, col] row direction
                    // printfn $"%A{table[*, col]} r: {row} c: {col} bi: {obstacleIndex} bitem: {obstacleItem}"
                    if obstacleIndex = 0 && obstacleItem = '.'
                    then 
                        table[0, col] <- 'O' 
                        table[row,col] <- '.'
                    else if obstacleIndex < (row - 1) 
                    then 
                        table[obstacleIndex+1, col] <- 'O'
                        table[row,col] <- '.'

                    // printfn $"row: {row} col: {col} value: {table[row,col]}"
    | W -> 
        for row in 0..rowCount-1 do
            for col in 0..colCount-1 do
                if table[row,col] = 'O' 
                then 
                    let obstacleIndex, obstacleItem = indexOfObstacle table[row, *] col direction
                    // printfn $"%A{table[*, col]} r: {row} c: {col} bi: {obstacleIndex} bitem: {obstacleItem}"
                    if obstacleIndex = 0 && obstacleItem = '.'
                    then 
                        table[row, 0l] <- 'O' 
                        table[row,col] <- '.'
                    else if obstacleIndex < (col - 1) 
                    then 
                        table[row, obstacleIndex+1] <- 'O'
                        table[row,col] <- '.'

                    // printfn $"row: {row} col: {col} value: {table[row,col]}"
    | S -> 
        for row in rowCount-1..-1..0 do
            for col in 0..colCount-1 do
                if table[row,col] = 'O' 
                then 
                    let obstacleIndex, obstacleItem = indexOfObstacle table[*,col] row direction
                    if obstacleIndex = (rowCount - 1) && obstacleItem = '.'                
                    then  
                        table[rowCount-1, col] <- 'O' 
                        table[row,col] <- '.'
                    else if obstacleIndex > row + 1 
                    then
                        table[obstacleIndex-1, col] <- 'O'
                        table[row,col] <- '.'
    
    | E -> 
        for row in 0..rowCount-1 do
            for col in colCount-1..-1..0 do
                if table[row,col] = 'O' 
                then 
                    // printfn $"R: {row} C: {col} D: {direction}"
                    // printf "\tBefore: %A" table[row,*]
                    let obstacleIndex, obstacleItem = indexOfObstacle table[row,*] col direction
                    // printfn $"\t {obstacleIndex},{obstacleItem}" 
                    if obstacleIndex = (colCount - 1) && obstacleItem = '.'                
                    then  
                        table[row, colCount-1] <- 'O' 
                        table[row,col] <- '.'
                    else if obstacleIndex > col + 1 
                    then
                        table[row, obstacleIndex-1] <- 'O'
                        table[row,col] <- '.'
                    // printfn "\t After: %A" table[row,*]
    
    table

[<TailCall()>]
let rec indexOfMovableNorth (line: char[]) (index : int ) =
    if index = line.Length - 1 || line[index] <> '.' 
    then (index,line[index])
    else indexOfMovable line (index+1)

[<TailCall()>]
let rec indexOfMovableSouth (line: char[]) (index: int) =
    if index = 0 || line.[index] <> '.'
    then (index, line.[index])    
    else indexOfMovableSouth line (index-1)


let tilt (direction: Dir) (table: char array2d) =
    let pointNowDot = HashSet<int*int>()
    let pointNowRock =HashSet<int*int>()
    let updateCol (line : char[])  colIndex =
        line |> Array.mapi (fun i value -> 
            if pointNowDot.Contains(i,colIndex)
            then '.'
            else if pointNowRock.Contains(i,colIndex)
            then 'O'
            else value)
    let updateRow (line : char[])  rowIndex =
        line |> Array.mapi (fun i value -> 
            if pointNowDot.Contains(rowIndex, i)
            then '.'
            else if pointNowRock.Contains(rowIndex,i)
            then 'O'
            else value)
    let updateValue row col value = 
        if pointNowDot.Contains(row,col) then '.'
        else if pointNowRock.Contains(row,col) then 'O'
        else value

    table
    |> Array2D.mapi (fun row col value -> 
        let updatedValue = updateValue row col value 
        if updatedValue = '.'
        then 
            let updatedVector = 
                match direction with
                | N | S -> updateCol table[*,col] col
                | E | W -> updateRow table[row,*] row
                
            let boundaryIndex,boundaryValue = 
                match direction with 
                | N -> indexOfMovableNorth updatedVector row
                | S -> indexOfMovableSouth updatedVector row

            if boundaryValue = 'O'
            then
                match direction with
                | N -> pointNowDot.Add(boundaryIndex,col) |> ignore
                | S -> pointNowDot.Add(boundaryIndex,col) |> ignore
                | E | W -> 
                    pointNowDot.Add(row,boundaryIndex) |> ignore
                
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
