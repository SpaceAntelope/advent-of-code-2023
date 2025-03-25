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

    let parse' path =
        path
        |> File.ReadAllLines
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
                    | Rt -> row, col + distance) (1L,1L)

        let minRow = points |> Array.minBy fst |> fst
        let minCol =  points |> Array.minBy snd |> snd
        let rowOffset = if minRow < 0L then - minRow else 0L
        let colOffset = if minCol < 0L then - minCol else 0L
        for index in 0..points.Length - 1 do
            let row,col = points.[index]
            points.[index] <- row + rowOffset, col + colOffset
        points

    type Vertex = { row: int64; col: int64}
    type SvgRect = { TopLeft: Vertex; Width: int64; Height: int64 }
    type Edge = { Start: Vertex; Stop: Vertex }

    let splitAreaToNonOverlappingSquares (data: (int64*int64) array) = 
        let edges = 
            data
            |> Array.pairwise

        let verticalEdges = 
            edges 
            |> Array.filter (fun ((r1,c1),(r2,c2)) -> c1 = c2)
            |> Array.map (fun ((r1,c1),(r2,c2))-> 
                match compare r1 r2 with
                | 1 -> { Start = { row = r2; col = c2}; Stop = { row = r1; col = c1} }
                | _ -> { Start = { row = r1; col = c1}; Stop = { row = r2; col = c2} })
            |> Array.sortBy (fun edge -> edge.Start.row)

        let expandedVerticalEdges =
            let rowsWithPoints = 
                data 
                |> Array.map fst 
                |> Array.distinct 
                |> Array.sort

            verticalEdges
            |> Array.collect (fun edge -> 
                rowsWithPoints 
                |> Array.filter (fun row -> edge.Start.row <= row && edge.Stop.row >= row)
                |> Array.pairwise 
                |> Array.map (fun (r1,r2) -> { Start = { row = r1; col = edge.Start.col }; Stop = { row = r2; col = edge.Start.col } })
            )
                
        expandedVerticalEdges
        |> Array.groupBy(fun edge -> edge.Start.row)
        |> Array.map snd
        |> Array.collect (fun edges -> 
            edges 
            |> Array.sortBy (fun e -> e.Start.col)
            |> Array.pairwise
            |> Array.indexed
            |> Array.filter (fun (i,_) -> i % 2 = 0)
            |> Array.map snd
            |> Array.map (fun (e1,e2) -> { TopLeft = e1.Start; Width = e2.Stop.col - e1.Start.col; Height = e2.Stop.row - e1.Start.row  })
        )

    let svgRectHtml (text:string) (rect: SvgRect) = 
         $"""
            <g>
                <!-- <rect width="{rect.Width}" height="{rect.Height}" x="{rect.TopLeft.col}" y="{rect.TopLeft.row}" fill="rgba(0,0,180,0.5)" stroke="fuchsia" style="stroke-width: 0.25; stroke-height: 0.25" /> -->
                <rect width="{rect.Width}" height="{rect.Height}" x="{rect.TopLeft.col}" y="{rect.TopLeft.row}" fill="rgba(0,0,180,0.5)" stroke="fuchsia" style="stroke-width: 0; stroke-height: 0" />
                <text x="{rect.TopLeft.col}" y="{rect.TopLeft.row + 1L}" font-size="1">{text}</text>
            </g>
        """

    let pointsToSvgPolygon (title: string) (data : (int64*int64) array) =
        let points = 
            data 
            |> Array.map (fun (row,col) -> sprintf "%d,%d" col row) 
            |> fun x -> String.Join (" ", x)
        let squares = splitAreaToNonOverlappingSquares data
        let rects = 
            squares 
            |> Array.mapi (fun i square-> svgRectHtml ((i+1).ToString()) square)
            |> String.concat "\n                "
        $$"""
        <div style="margin: 1em;border: 3px solid #CDDC39;border-radius: 1em;padding: 1em; width: 75%;">
            <label>Polygon</label>
            <label>{{title}}</label>
            <label>squares: {{squares.Length}}</label>
            <svg>
                <polygon 
                    points="{{points}}"
                    style="fill:lime;stroke:purple"/>
                <!-- {{squares.Length}} rects below -->
                {{rects}}
            </svg>
        </div>
        """

    let html() = 
        "./input/puzzle.example" 
        |> parse'
        |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
        |> pathToPoints
        |> pointsToSvgPolygon "Example, part 1"
        |> printfn "%s"
        
        "./input/puzzle.example" 
        |> parse     
        |> pathToPoints
        |> pointsToSvgPolygon "Example, part 2"
        |> printfn "%s"

        "./input/puzzle.input" 
        |> parse'
        |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
        |> pathToPoints
        |> pointsToSvgPolygon "Input part 1"
        |> printfn "%s"

        "./input/puzzle.input" 
        |> parse     
        |> pathToPoints
        |> pointsToSvgPolygon "Input part 2"
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

    // html()

    let pathLength (points: (int64*int64) array) = 
        points 
        |> Array.pairwise
        |> Array.sumBy Common.General.manhattan64

    let areafromPoints (points: (int64*int64) array) =                
        points
        |> fun points -> 

            points
            |> splitAreaToNonOverlappingSquares
            |> Array.sumBy (fun rect -> rect.Width * rect.Height)
            |> (+) ((pathLength points)/2L + 1L)
    
    "./input/puzzle.example" 
    |> parse'
    |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
    |> pathToPoints
    |> areafromPoints
    |> Common.Assertions.shouldBe 62

    "./input/puzzle.example" 
    |> parse     
    |> pathToPoints
    |> areafromPoints
    |> Common.Assertions.shouldBe 952408144115L

    "./input/puzzle.input" 
    |> parse     
    |> pathToPoints
    |> areafromPoints
    |> printfn "The lagoon can hold %d cubic meters of lava" 