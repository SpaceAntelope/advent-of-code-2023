#load "./gear-ratios-common.fsx" 

open System
open ``Gear-ratios-common``

let testData = @"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

let expectedComponents = [467;35;633;617;592;755;664;598] |> List.sort
let expectedSum = 4361
let table = parse testData


printfn "--- test area ---"
printfn "%A" table
let area22 = area 2 2

table
|> Array2D.mapi (fun row  col z -> if (area22 |> List.contains (row,col)) then "💕" else (string z)) 
|> printfn "\n%A"

let actualComponents = components table |> List.sort
let actualSum = actualComponents |> List.sum
if actualComponents <> expectedComponents then failwith $"Components: Expected {expectedComponents} but got {actualComponents}"
if actualSum <> expectedSum then failwith $"Component sum: Expected {expectedSum} but got {actualSum}"

    
let expectedTotalGearRatio = 467835
let actualTotalGearRatio = gearRatios table |> List.sum

if expectedTotalGearRatio <> actualTotalGearRatio then failwith $"Gear ratio: Expected {expectedTotalGearRatio} but got {actualTotalGearRatio}"
