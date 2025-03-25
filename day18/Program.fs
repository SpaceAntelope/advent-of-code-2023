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

    // type Edge = {
    //     Start : int64*int64
    //     Stop : int64*int64
    //     InnerDir: Dir
    // }
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
                    | Rt -> row, col + distance) (1L,1L)

        let minRow = points |> Array.minBy fst |> fst
        let minCol =  points |> Array.minBy snd |> snd
        let rowOffset = if minRow < 0L then - minRow else 0L
        let colOffset = if minCol < 0L then - minCol else 0L
        for index in 0..points.Length - 1 do
            let row,col = points.[index]
            points.[index] <- row + rowOffset, col + colOffset
        points

    let pathToEdges (data: (Dir*int64) array) =
        let edges = 
            data 
            |> Array.scan (fun (row,col) (dir,distance) -> 
                    match dir with 
                    | Dn -> row + distance, col
                    | Up -> row - distance, col
                    | Lt -> row, col - distance
                    | Rt -> row, col + distance) (0L,0L)
            |> Array.pairwise 
        
        let verticalEdges = 
            edges 
            |> Array.filter (fun ((r1,c1),(r2,c2)) -> c1 = c2) 
            |> Array.map (fun ((r1,c1),(r2,c2)) -> r1,r2,c1)
        
        // let leftMostVerticalEdge = verticalEdges |> Array.minBy (fun (_,_,c)->c) 
        ()

    let rebasePoints (size:float) (data : (int64*int64) array) =
        let maxRow = data |> Array.maxBy fst |> fst |> float
        let maxCol = data |> Array.maxBy snd |> snd |> float
        data |> Array.map (fun (row,col) -> float row * size / maxRow, float col * size / maxCol )

    type Point = int64*int64
    type SplitAreaState = { PrevRowPoints: Point array; Squares: Point array array }
    type Vertex = { row: int64; col: int64}
    type SvgRect = { TopLeft: Vertex; Width: int64; Height: int64 }
    type Edge = { Start: Vertex; Stop: Vertex } with override x.ToString() = x.ToString().Replace('\n',' ')

    let splitAreaToNonOverlappingSquares (data: (int64*int64) array) = 
        let edges = 
            // [|data |> Array.head |]
            // |> Array.append data
            data
            |> Array.pairwise
            |> Common.General.tee2 "Edges"

        let verticalEdges = 
            edges 
            |> Array.filter (fun ((r1,c1),(r2,c2)) -> c1 = c2)
            |> Array.map (fun ((r1,c1),(r2,c2))-> 
                match compare r1 r2 with
                | 1 -> { Start = { row = r2; col = c2}; Stop = { row = r1; col = c1} }
                | _ -> { Start = { row = r1; col = c1}; Stop = { row = r2; col = c2} })
            |> Array.sortBy (fun edge -> edge.Start.row)
            |> Common.General.tee2 "Vertical Edges"

        let expandedVerticalEdges =
            let rowsWithPoints = data |> Array.map fst |> Array.distinct |> Array.sort |> Common.General.tee2 "Rows with points"
            verticalEdges
            |> Array.collect (fun edge -> 
                rowsWithPoints 
                |> Array.filter (fun row -> 
                    let result = edge.Start.row <= row && edge.Stop.row >= row
                    // printfn "Row: %d Edge: %A Result: %b" row edge result
                    result
                    )
                |> Array.pairwise 
                |> Array.map (fun (r1,r2) -> { Start = { row = r1; col = edge.Start.col }; Stop = { row = r2; col = edge.Start.col } })
            )
            |> Common.General.tee2 "Expanded Vertical Edges"
        
        // let sortedData = 
        //     data
        //     |> fun data ->
        //         if data.[0] = data.[^0]
        //         then data |> Array.skip 1
        //         else data
        //     |> Array.groupBy fst        
        //     |> Array.map (fun (key,grp)-> key, grp |> Array.sortBy snd)
        //     |> Array.sortBy fst
        //     |> Array.map snd
                
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

    let splitAreaToNonOverlappingSquares' (data: (int64*int64) array) = 
        // let directedEdges =
        //     let edges = data |> Array.pairwise //|> Array.map (fun (p1,p2) -> { Start = p1; Stop = p2; InnerDir = Dn })
        //     let topRow = data |> Array.minBy fst |> fst            
        //     let topHorizontalEdgeIndex= edges |> Array.findIndex (fun (p1,p2) -> topRow = fst p1 && topRow = fst p2)
        //     let edgesStartingFromTopHorizontalEdge= edges.[..topHorizontalEdgeIndex-1] |> Array.append edges1.[topHorizontalEdgeIndex..]
        //     let (r1,c1),(r2,c2) = edges.[topHorizontalEdgeIndex]
        //     let innerDirection = 
        //         if c1 < c2 then Dn else Up
        //     edgesStartingFromTopHorizontalEdge
        //     |> Array.skip 1
        //     |> Array.scan (fun state current ->
        //         let (r1,c1),(r2,c2) = current
        //         match compare r1 r2, compare c1 c2, state.InnerDir with
        //         | rDiff, cDiff, prevDir -> 
        //          ) { Start = (r1,c1); Stop = (r2,c2); InnerDir = innerDirection }
        
        // let flip (x,y) = y,x
        let edges = data |> Array.pairwise // |> Array.collect (fun e -> [|e;flip e|] )

        let isWithinEdge (((r1,c1): Point), ((r2,c2): Point)) = 
            let minRow = Math.Min(r1,r2)
            let minCol = Math.Min(c1,c2)
            let maxRow = Math.Max(r1,r2)
            let maxCol = Math.Max(c1,c2)            
            match compare r1 r2, compare c1 c2 with
            | 0, 0 -> failwith $"It's not and edge if both points are the same ({(r1,c1)} {(r2,c2)})"
            | 0, _ -> edges |> Array.tryFind (fun ((y1,x1),(y2,x2)) -> r1 = y1 && minCol >= Math.Min(c1,c2) && maxCol <= Math.Max(c1,c2))
            | _, 0 -> edges |> Array.tryFind (fun ((y1,x1),(y2,x2)) -> c1 = x1 && minRow >= Math.Min(y1,y2) && maxRow <= Math.Max (y1,y2))             
            | _, _ -> failwith $"No diagonal edges allowed ({(r1,c1)} {(r2,c2)})"
            |> fun x -> 
                if x.IsSome 
                then printfn "<div style='margin-left: 5px'>%A %A € %A</div>" (r1,c1) (r2,c2) x                
                x.IsSome
        
        let printPointTable (data : Point array) = 
            let maxRow = data |> Array.maxBy fst |> fst |> int |> (+) 1
            let maxCol = data |> Array.maxBy snd |> snd |> int |> (+) 1
            let map = Array2D.init maxRow maxCol (fun row col -> if data |> Array.contains (row,col) then Some (row,col) else None)
            printfn "<table style='border: 1px solid black'>"
            for row in 0..maxRow-1 do
                printfn "<tr>"
                for col in 0..maxCol-1 do
                    if map.[row,col].IsSome then
                        printfn "<td style='background-color: blue; color: white; font-weight: bold'>%d %d</td>" row col
                    else 
                        printfn "<td style='border: 1px solid black'>%s %s</td>" "." "."
                printfn "</tr>"
            printfn "</table>"

        // printPointTable data

        data
        |> fun data ->
            if data.[0] = data.[^0]
            then data |> Array.skip 1
            else data
        |> Array.groupBy fst        
        |> Array.map (fun (key,grp)-> key, grp |> Array.sortBy snd)
        |> Array.sortBy fst
        |> Array.map snd
        |> Common.General.tee2 "Groupd poitns"
        |> Array.fold (fun (state: SplitAreaState) currentRowPoints -> 
                printfn "<br/>"
                [
                    $"Squares: {state.Squares.Length}"
                    $"Row: {currentRowPoints |> Array.head |> fst}"
                    $"Prev Points: %A{state.PrevRowPoints}"
                    $"Curr Points: %A{currentRowPoints}"
                ] |> List.iter(printfn "<div>%s</div>")
                
                let points = state.PrevRowPoints // |> Array.append currentRowPoints |> Array.sortBy snd |> Array.distinct
                match points with
                | [||] -> { state with PrevRowPoints = currentRowPoints }
                | prevRow ->
                    let currentRowIndex = currentRowPoints |> Array.head |> fst
                    let prevRowIndex = prevRow |> Array.head |> fst
                    let height = currentRowIndex - prevRowIndex
                    let squares = 
                        prevRow 
                        |> Array.pairwise
                        |> Array.choose (fun ((r1,c1),(r2,c2)) ->
                            // printfn "%A" <| (r1,c1,r2,c2)
                            let square = 
                                match compare c1 c2 with
                                | 0 -> failwith $"It's not and edge if both points are the same ({(r1,c1)} {(r2,c2)})"
                                | 1 ->  [|prevRowIndex,c2;prevRowIndex,c1;currentRowIndex,c1;currentRowIndex,c2|]
                                | -1 -> [|prevRowIndex,c1;prevRowIndex,c2;currentRowIndex,c2;currentRowIndex,c1|]
                                | x -> failwith $"Should happen {x}"
                            

                            match (square |> Array.pairwise |> Array.forall isWithinEdge) && (isWithinEdge (square.[3],square.[0])) with
                            | true -> Some square
                            | false -> 
                                printfn "<div style='font-weight: bold'>Rejected: %A</div>" square
                                None
                        )
                    
                    printfn "<ol>"
                    for square in squares do printfn $"<li>%A{square}</li>"
                    printfn "</ol>"
                    let points = 
                        squares 
                        |> Array.concat 
                        |> Array.filter (fun (row, col) -> row = currentRowIndex) 
                        |> Array.append currentRowPoints
                        |> Array.distinct
                        |> Array.sortBy snd
                    { PrevRowPoints = points; Squares = squares |> Array.append state.Squares }
                ) { PrevRowPoints = [||]; Squares = [||] }
        |> _.Squares

    let pointsToRect (points: Point[]) =
        let leftCol = points |> Array.minBy snd |> snd
        let topRow = points |> Array.minBy fst |> fst
        let rightCol = points |> Array.maxBy snd |> snd
        let bottomRow = points |> Array.maxBy fst |> fst
        let height = bottomRow - topRow
        let width = rightCol - leftCol

        $"""
            <rect width="{width}" height="{height}" x="{leftCol}" y="{topRow}" fill="rgba(0,0,180,0.5)" stroke="fuchsia" style="stroke-width: 0.25; stroke-height: 0.25" />
        """
    let pointsToRectWithText (text:string) (points: Point[]) =
        let leftCol = points |> Array.minBy snd |> snd
        let topRow = points |> Array.minBy fst |> fst
        let rightCol = points |> Array.maxBy snd |> snd
        let bottomRow = points |> Array.maxBy fst |> fst
        let height = bottomRow - topRow
        let width = rightCol - leftCol

        $"""
            <g>
                <rect width="{width}" height="{height}" x="{leftCol}" y="{topRow}" fill="rgba(0,0,180,0.5)" stroke="fuchsia" style="stroke-width: 0.25; stroke-height: 0.25" />
                <text x="{leftCol}" y="{topRow + 1L}" font-size="1">{text}</text>
            </g>
        """

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
            // |> rebasePoints size
            // |> Array.map (fun (row,col) -> sprintf "%.3f,%.3f" row col) 
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
    let html() = 
        "./input/puzzle.example" 
        |> parse'
        |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
        |> pathToPoints
        |> pointsToSvgPolygon "Example, part 1"
        |> printfn "%s"

        // "./input/puzzle.example" 
        // |> parse'
        // |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)
        // |> fun instr -> 
        //     instr  
        //     |> instrToSvgPath "Example, part 1"
        //     |> printfn "%s"
        
        "./input/puzzle.example" 
        |> parse     
        |> pathToPoints
        |> pointsToSvgPolygon "Example, part 2"
        |> printfn "%s"

        // // "./input/puzzle.example" 
        // // |> parse
        // // |> fun instr -> 
        // //     instr  
        // //     |> instrToSvgPath "Example part 2"
        // //     |> printfn "%s"

        //     // instr
        // // |> pathToPoints
        // // |> pointsToSvgPolygon 1000
        // // |> printfn "%s"
        // // |> calculateArea
        // // |> Common.Assertions.shouldBe 952408144115L

        
        "./input/puzzle.input" 
        |> parse'
        |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)  
        |> pathToPoints
        |> pointsToSvgPolygon "Input part 1"
        |> printfn "%s"

        // // "./input/puzzle.input" 
        // // |> parse'
        // // |> Array.map (fun instr -> instr.Direction, int64 instr.Steps)
        // // |> fun instr -> 
        // //     instr  
        // //     |> instrToSvgPath "Input part 1"
        // // |> printfn "%s"

        "./input/puzzle.input" 
        |> parse     
        |> pathToPoints
        |> pointsToSvgPolygon "Input part 2"
        |> printfn "%s"
        
        // // "./input/puzzle.input" 
        // // |> parse
        // // |> fun instr -> 
        // //     instr  
        // //     |> instrToSvgPath "Input part 2"
        // // |> printfn "%s"

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
        |> Common.General.tee2 "Points"
        |> fun points -> 
            printfn "Trench length: %d" (pathLength points)

            points
            |> splitAreaToNonOverlappingSquares
            |> Common.General.tee2 "Squares"        
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