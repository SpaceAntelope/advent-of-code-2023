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

    type Edge = {
        Start : int64*int64
        Stop : int64*int64
    }
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

    let parse' path =
        path
        |> File.ReadAllLines
        // data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map (fun line ->
            let parts = line.Split(' ')

            {   Direction = Dir.FromChar (parts.[0].[0])
                Steps = int parts.[1]
                Hex = parts[2].Trim([| '('; ')'; '#' |]) })

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
        let points = 
            data 
            |> Array.scan (fun (row,col) (dir,distance) -> 
                    match dir with 
                    | Dn -> row + distance, col
                    | Up -> row - distance, col
                    | Lt -> row, col - distance
                    | Rt -> row, col + distance) (0L,0L)

        let minRow = points |> Array.minBy fst |> fst
        let minCol =  points |> Array.minBy snd |> snd
        let rowOffset = if minRow < 0L then - minRow else 0L
        let colOffset = if minCol < 0L then - minCol else 0L
        for index in 0..points.Length - 1 do
            let row,col = points.[index]
            points.[index] <- row + rowOffset, col + colOffset
        points

    let rebasePoints (size:float) (data : (int64*int64) array) =
        let maxRow = data |> Array.maxBy fst |> fst |> float
        let maxCol = data |> Array.maxBy snd |> snd |> float
        data |> Array.map (fun (row,col) -> float row * size / maxRow, float col * size / maxCol )


    let pointsToSvgPolygon (title: string) (data : (int64*int64) array) =
        let points = 
            data 
            // |> rebasePoints size
            // |> Array.map (fun (row,col) -> sprintf "%.3f,%.3f" row col) 
            |> Array.map (fun (row,col) -> sprintf "%d,%d" col row) 
            |> fun x -> String.Join (" ", x)
        $$"""
        <div style="margin: 1em;border: 3px solid #CDDC39;border-radius: 1em;padding: 1em;">
            <label>Polygon</label>
            <label>{{title}}</label>
            <svg>
                <polygon 
                    points="{{points}}"
                    style="fill:lime;stroke:purple;stroke-width:1;stroke-height:1;fill-rule:evenodd;">
            </svg>
        </div>
        """
    
    let instrToSvgPath (title:string) (data: (Dir*int64) array) = 
        let d = 
            data 
            |> Array.map (fun (dir,steps) -> 
                match dir with
                | Up -> $"v {-steps}"
                | Dn -> $"v {steps}"
                | Rt -> $"h {steps}"
                | Lt -> $"h {-steps}")
            |> String.concat " "
        $$"""
        <div style="margin: 1em;border: 3px solid #CDDC39;border-radius: 1em;padding: 1em;">
            <label>Path</label>
            <label>{{title}}</label>
            <svg>
                <path 
                    d="M 0 0 {{d}}"
                    style="fill:lime;stroke:purple;stroke-width:1;stroke-height:1;fill-rule:evenodd;">
            </svg>
        </div>
        """

    (*


// Update viewBox to contain the entire path
svgElement.setAttribute('viewBox', 
  `${bbox.x} ${bbox.y} ${bbox.width} ${bbox.height}`);
    *)

    // more like manhattan distance between first and last point, i.e. 0
    let size (data: (Dir*int64) array) =
        Array.fold (fun (rows,cols) (dir,dist) -> 
            printfn "%A" (rows,cols,dir,dist)
            match dir with 
            | Dn -> rows + dist, cols
            | Up -> rows - dist, cols
            | Lt -> rows, cols - dist
            | Rt -> rows, cols + dist) (0L,0L) data

    let calculateArea (data: (int64*int64) array) = 
        data
        |> Array.groupBy fst        
        |> Array.map (fun (key,grp)-> key, grp |> Array.sortBy snd)
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.fold (fun (prevRows, area) currentRowPoints -> 
                match prevRows with
                | [||] -> currentRowPoints, area
                | prevRow ->
                    let currentRowIndex = currentRowPoints |> Array.head |> fst
                    let prevRowIndex = prevRow |> Array.head |> fst
                    let height = currentRowIndex - prevRowIndex
                    let additionalArea = 
                        prevRow 
                        |> Array.chunkBySize 2 
                        |> Array.sumBy (fun chunk -> 
                            let c1 = chunk.[0] |> snd
                            let c2 = chunk.[1] |> snd
                            let width = c2 - c1
                            height * width)
                    currentRowPoints, area + additionalArea                    
                ) ([||],0L)
        |> snd
        // |> Array.iter (printfn "%A")

    "./input/puzzle.example" 
    |> parse'
    |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
    |> pathToPoints
    |> pointsToSvgPolygon "Example, part 1"
    |> printfn "%s"

    "./input/puzzle.example" 
    |> parse'
    |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)
    |> fun instr -> 
        instr  
        |> instrToSvgPath "Example, part 1"
        |> printfn "%s"
    
    "./input/puzzle.example" 
    |> parse     
    |> pathToPoints
    |> pointsToSvgPolygon "Example, part 2"
    |> printfn "%s"

    "./input/puzzle.example" 
    |> parse
    |> fun instr -> 
        instr  
        |> instrToSvgPath "Example part 2"
        |> printfn "%s"

        // instr
    // |> pathToPoints
    // |> pointsToSvgPolygon 1000
    // |> printfn "%s"
    // |> calculateArea
    // |> Common.Assertions.shouldBe 952408144115L

    
    "./input/puzzle.input" 
    |> parse'
    |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
    |> pathToPoints
    |> pointsToSvgPolygon "Input part 1"
    |> printfn "%s"

    "./input/puzzle.input" 
    |> parse'
    |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)
    |> fun instr -> 
        instr  
        |> instrToSvgPath "Input part 1"
    |> printfn "%s"

    "./input/puzzle.input" 
    |> parse     
    |> pathToPoints
    |> pointsToSvgPolygon "Input part 2"
    |> printfn "%s"
    
    "./input/puzzle.input" 
    |> parse
    |> fun instr -> 
        instr  
        |> instrToSvgPath "Input part 2"
    |> printfn "%s"

    """
<script>
    const svgElements = document.querySelectorAll('svg');
    for (const svgElement of svgElements) {
        const path = svgElement.querySelector('path,polygon');
        if (path) {
            const bbox = path.getBBox();
            svgElement.setAttribute('viewBox', `${bbox.x} ${bbox.y} ${bbox.width} ${bbox.height}`);
        }
    }
</script>
    """ |> printfn "%s"