module Day13.Program

open System
open System.IO
open System.Text.RegularExpressions
open Common

type Symmetry = 
    | V of (int*int) 
    | H of (int*int)
    with 
        member x.Summarize() =
            match x with 
            | V(r1,r2) -> (r1 + 1) * 100
            | H(c1,c2) -> c1 + 1

type Tile = Rocks | Ash
    with 
        member x.AsChar = 
            match x with Rocks -> '#' | Ash -> '.'
        static member FromChar x = 
            match x with '.' -> Ash | '#' -> Rocks | z -> failwith $"Unknown tile {z}"

type Area = Tile array2d

module Area =
    let AsChar (area: Area) =
        let rows,cols = Common.Matrix.size area
        Array2D.init rows cols (fun row col -> area.[row,col].AsChar)

let parse path : Area[] =
    path
    |> File.ReadAllText
    |> fun text -> Regex.Split(text, @"^\s*$", RegexOptions.Multiline)
    |> Array.map (fun patt -> 
        patt.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map (fun line -> line.ToCharArray() |> Array.map Tile.FromChar)
        |> array2D)

let printPatt (patt: Area) =
    Matrix.printBase (Area.AsChar patt ) (Seq.empty)

let CheckLeftRightSymmetry<'T when 'T : equality> (matrix1 : 'T array2d) (matrix2 : 'T array2d) =
    let rows,cols = Matrix.size matrix1
    matrix1
    |> Matrix.allIndices
    |> Seq.tryFind (fun (row,col) -> matrix1.[row,col] <> matrix2.[row, cols-col-1])
    |> function 
    | Some _ -> false
    | None -> true

let checkTopBottomSymmetry<'T when 'T : equality> (top : 'T array2d) (bottom : 'T array2d) =
    let rows,cols = Matrix.size top
    top     
    |> Matrix.allIndices
    |> Seq.tryFind (fun (row,col) -> top.[row,col] <> bottom.[rows - row - 1, col])
    |> function 
    | Some _ -> false
    | None -> true

let findLeftRightSymmetryAxis (area: Area) =
    let area = Area.AsChar area
    let rows,cols = Matrix.size area
    [|0..cols-1|] 
    |> Array.pairwise 
    |> Array.where (fun (c1,c2) -> area[*,c1] = area[*,c2])
    |> Array.tryFind (fun (c1,c2) -> 
        let width = Math.Min(c1+1, cols - c2)
        let left = area.[*, c1-width+1..c1]
        let right = area.[*, c2..c2+width-1]

        CheckLeftRightSymmetry left right)
    |> Option.map (fun point -> H point)
        
let findTopBottomSymmetryAxis (area: Area) =
    let area = Area.AsChar area
    let rows,cols = Matrix.size area
    [|0..rows-1|] 
    |> Array.pairwise 
    |> Array.where (fun (r1,r2) -> area[r1,*] = area[r2,*])
    |> Array.tryFind (fun (r1,r2) -> 
        let height = Math.Min(r1+1, rows - r2)
        let top = area.[r1-height+1..r1,*]
        let bottom = area.[r2..r2+height-1, *]

        checkTopBottomSymmetry top bottom)
    |> Option.map (fun point -> V point)

let symmetry (area: Area) =
    [|  findTopBottomSymmetryAxis area
        findLeftRightSymmetryAxis area |]
    |> Array.choose id

"./input/puzzle.example"
|> parse 
|> Array.collect symmetry
|> Array.sumBy _.Summarize()
|> Assertions.shouldBe 405

"./input/puzzle.input"
|> parse 
|> Array.collect symmetry
|> fun x -> printfn $"Found {x.Length} patterns."; x
|> Array.sumBy _.Summarize()
|> printfn "Pattern summary is %A"

let haveOneTileDifferent<'T when 'T : equality> (a: 'T[]) (b: 'T[])  =
    let mutable diffCount = 0
    let mutable index = 0
    let mutable diffIndex = -1
    
    while diffCount < 2 && index < a.Length do
        if a.[index] <> b.[index] 
        then 
            diffCount <- diffCount + 1
            diffIndex <- index
        index <- index + 1

    if diffCount = 1
    then Some diffIndex
    else None


let rec symmetryPairs (arr: 'a array) =
    let head = arr |> Array.head
    let tail= arr |> Array.tail
    match tail with 
    | [||] -> [||]
    | _ -> 
        tail
        |> Array.map (fun x -> head, x)
        |> Array.where( fun (x,y)  -> (y-x)%2=0)
        |> Array.append (symmetryPairs tail)

let CheckLeftRightSymmetry2<'T when 'T : equality> (matrix : 'T array2d)
    let rows,cols = Matrix.size matrix1
    matrix1
    |> Matrix.allIndices
    |> Seq.tryFind (fun (row,col) -> matrix1.[row,col] <> matrix2.[row, cols-col-1])
    |> function 
    | Some _ -> false
    | None -> true

// let findLeftRightSymmetryAxis2 (area: Area) =
//     let area = Area.AsChar area
//     let rows,cols = Matrix.size area
//     [|0..cols-1|]
//     |> symmetryPairs
//     |> Array.choose (fun (c1,c2) -> 
//         haveOneTileDifferent (area[*,c1]) (area[*,c2]) 
//         |> Option.map (fun index -> c1,c2,index))
    
//     |> Array.pairwise 
//     |> Array.where (fun (c1,c2) -> area[*,c1] = area[*,c2])
//     |> Array.tryFind (fun (c1,c2) -> 
//         let width = Math.Min(c1+1, cols - c2)
//         let left = area.[*, c1-width+1..c1]
//         let right = area.[*, c2..c2+width-1]

//         CheckLeftRightSymmetry left right)
//     |> Option.map (fun point -> H point)
        