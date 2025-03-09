namespace Day18

module Main =
    open System
    open System.IO

    type Dir = 
        Lt | Rt | Up | Dn
        with 
            static member FromChar c =
                match c with
                | 'R' -> Rt
                | 'D' -> Dn
                | 'L' -> Lt
                | 'U' -> Up
                | '0' -> Rt
                | '1' -> Dn
                | '2' -> Lt
                | '3' -> Up
                | x -> failwith $"Unknown direction character '{x}'"
          
    type Instruction = {
        Direction : Dir
        Steps: int
        Hex: string
    } with 
        member x.Corrected() = 
            let distance = 
                x.Hex.Substring(0,5)
                |> fun hex -> Convert.ToInt64(hex,16)
            let dir = x.Hex.Substring(5).[0] |> Dir.FromChar
            dir, distance

    let parse path =
        path
        |> File.ReadAllLines
        |> Array.map ( fun line -> 
            let parts = line.Split(' ')
            {   Direction = Dir.FromChar (parts.[0].[0])
                Steps = int parts.[1]
                Hex = parts.[2].Trim([|'#';'(';')'|]) })
        |> Array.map _.Corrected()

    let pathToPoints (data: (Dir*int64) array) =
        data 
        |> Array.scan (fun (row,col) (dir,distance) -> 
                match dir with 
                | Dn -> row + distance, col
                | Up -> row - distance, col
                | Lt -> row, col - distance
                | Rt -> row, col + distance) (0L,0L)
        |> Array.skip 1

    let size (data: (Dir*int64) array) =
        Array.fold (fun (rows,cols) (dir,dist) -> 
            printfn "%A" (rows,cols,dir,dist)
            match dir with 
            | Dn -> rows + dist, cols
            | Up -> rows - dist, cols
            | Lt -> rows, cols - dist
            | Rt -> rows, cols + dist) (0L,0L) data
        

    

    // let calculateArea (data: (int64*int64) array) = 
    //     data
    //     |> Array.groupBy fst        
    //     |> Array.map (fun (key,grp)-> key, grp |> Array.sortBy snd)
    //     |> Array.sortBy fst
    //     |> Array.map snd
    //     |> Array.fold (fun (prevRows, area) currentRowPoints -> 

    //     ) ([||],0L)

    //     |> Array.iter (printfn "%A")

    "./input/puzzle.example" 
    |> parse     
    |> pathToPoints
    |> calculateArea
    // |> Array.iter (printfn "%A")        