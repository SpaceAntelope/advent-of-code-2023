#load "./rocks-common.fsx"

open System
open System.IO
open System.Collections.Generic
open ``Rocks-common``

let testData = @"
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."





let testTiltNorth = 
    let expectedTable = parse @"OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#...." 
    let expectedScore = 136

    testData
    |> parse
    |> tiltNorth
    |> function 
    | actual when expectedTable = actual -> printfn "Tilt north test passed"; actual
    | actual -> failwith $"Tilt north test failed. Expected \n%A{expectedTable}\n but got \n%A{actual}\n"
    |> score
    |> function
    | actual when expectedScore = actual -> printfn "Score calcualtion passed"
    | actual -> failwith $"Score calculation failed: Expected {expectedScore} but got {actual}."


type TestCase = { Col: char[]; ExpectedS: (int*int*char)[]; ExpectedN: (int*int*char)[] }
let testIndexOfObstacle1 = 
    let testCases = [
        {   Col = [|'O'; 'O'; '.'; 'O'; '.'; 'O'; '.'; '.'; '#'; '#'|] 
            ExpectedN = [|0,0,'O';1,0,'O'; 2,1,'O';3,1,'O';4,3,'O';5,3,'O';6,5,'O';7,5,'O';8,5,'O';9,8,'#' |]
            ExpectedS = [|0,1,'O';1,3,'O'; 2,3,'O';3,5,'O';4,5,'O';5,8,'#';6,8,'#';7,8,'#';8,9,'#';9,9,'#' |]
        }; {
            Col =  [|'.'; 'O'; '.'; '.'; '.'; '#'; 'O'; '.'; '.'; 'O'|]
            ExpectedN = [|0,0,'.';1,0,'.'; 2,1,'O';3,1,'O';4,1,'O';5,1,'O';6,5,'#';7,6,'O';8,6,'O';9,6,'O' |]
            ExpectedS = [|0,1,'O';1,5,'#'; 2,5,'#';3,5,'#';4,5,'#';5,6,'O';6,9,'O';7,9,'O';8,9,'O';9,9,'O' |]
        };{
            Col = [|'O'; 'O'; '.'; '#'; 'O'; '.'; '.'; '.'; '.'; 'O'|]
            ExpectedN = [|0,0,'O';1,0,'O'; 2,1,'O';3,1,'O';4,3,'#';5,4,'O';6,4,'O';7,4,'O';8,4,'O';9,4,'O' |]
            ExpectedS = [|0,1,'O';1,3,'#'; 2,3,'#';3,4,'O';4,9,'O';5,9,'O';6,9,'O';7,9,'O';8,9,'O';9,9,'O' |]
        }
    ]
   
    let test col index expectedBoundaryIndex expectedBoundaryValue dir =
        match indexOfObstacle col index dir with
        | actualBoundaryIndex, actualBoundaryValue 
            when expectedBoundaryIndex = actualBoundaryIndex && expectedBoundaryValue = actualBoundaryValue ->
                printfn $"Obstacle discovery {dir} passed for index {index}"
        | actualBoundaryIndex, actualBoundaryValue ->
            failwith $"Obstacle discovery {dir} in\n\t%A{col}\nfailed for index {index}: expected {expectedBoundaryIndex},{expectedBoundaryValue} but got {actualBoundaryIndex},{actualBoundaryValue}"
    
    testCases 
    |> List.iteri (fun i testCase ->             
            printfn "Case %d: %A" (i+1) testCase.Col
            testCase.ExpectedN
            |> Array.iter (fun (rowIndex,expectedBoundaryIndex,expectedBoundaryValue) 
                            -> test testCase.Col rowIndex expectedBoundaryIndex expectedBoundaryValue N)
            testCase.ExpectedS
            |> Array.iter (fun (rowIndex,expectedBoundaryIndex,expectedBoundaryValue) 
                            -> test testCase.Col rowIndex expectedBoundaryIndex expectedBoundaryValue S)    
            testCase.ExpectedN
            |> Array.iter (fun (rowIndex,expectedBoundaryIndex,expectedBoundaryValue) 
                            -> test testCase.Col rowIndex expectedBoundaryIndex expectedBoundaryValue W)
            testCase.ExpectedS
            |> Array.iter (fun (rowIndex,expectedBoundaryIndex,expectedBoundaryValue) 
                            -> test testCase.Col rowIndex expectedBoundaryIndex expectedBoundaryValue E)
    
    )
    
let testCases = 
    [
        N, parse @"OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#...." 
        S, parse @"
.....#....
....#....#
...O.##...
...#......
O.O....O#O
O.#..O.#.#
O....#....
OO....OO..
#OO..###..
#OO.O#...O
"
        E, parse @"
....O#....
.OOO#....#
.....##...
.OO#....OO
......OO#.
.O#...O#.#
....O#..OO
.........O
#....###..
#..OO#....
"
        W, parse @"
O....#....
OOO.#....#
.....##...
OO.#OO....
OO......#.
O.#O...#.#
O....#OO..
O.........
#....###..
#OO..#...."
    ]     
let testTiltByDirection() = 
   
    let test dir expected = 
        testData
        |> parse
        |> tilt dir
        |> function 
        | actual when expected = actual -> printfn $"Tilt by direction {dir} test passed"; actual
        | actual -> failwith $"Tilt by direction {dir} test failed. Expected \n%A{expected}\n but got \n%A{actual}\n"
        
    testCases
    |> List.iter (fun (dir,table) -> test dir table |> ignore)

testData
|> parse
|> printfn "%A" 

let testTiltByDirectionInPlace = 
   
    let test dir expected = 
        testData
        |> parse
        |> tiltInPlace dir
        |> function 
        | actual when expected = actual -> printfn $"Tilt by direction {dir} in place test passed"; actual
        | actual -> failwith $"Tilt by direction {dir} in place test failed. Expected \n%A{expected}\n but got \n%A{actual}\n"
        
    testCases
    |> List.iter (fun (dir,table) -> test dir table |> ignore)

