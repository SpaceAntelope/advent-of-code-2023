namespace Common

    module Matrix =
        open System.Text.RegularExpressions
        open System.Text

        let allIndices matrix =
                let rows = matrix |> Array2D.length1
                let cols = matrix |> Array2D.length2
                ([0..rows-1],[0..cols-1])
                ||> Seq.allPairs

        let findCell predicate matrix = 
            matrix
            |> allIndices
            |> Seq.find (fun (row,col) -> predicate <| matrix.[row, col])
        
        let tryFindCell predicate matrix = 
            matrix
            |> allIndices
            |> Seq.tryFind (fun (row,col) -> predicate <| matrix.[row, col])

        let size (matrix : 'a array2d) =
            let rows = matrix |> Array2D.length1
            let cols = matrix |> Array2D.length2
            rows,cols

        let simplePrint (matrix : 'a array2d) =
            let rows,cols = size matrix

            for row in 0..rows-1 do
                for col in 0..cols-1 do
                    printf " %O" matrix.[row,col]
                printfn ""

        let simplePrintUnspaced (matrix : 'a array2d) =
            let rows,cols = size matrix

            for row in 0..rows-1 do
                for col in 0..cols-1 do
                    printf "%O" matrix.[row,col]
                printfn ""

        let isOutOfBounds (size: int*int) (point: int*int) =
                let rows,cols = size
                let row,col = point

                row > rows-1 || col > cols-1 || col < 0 || row < 0
        
        let isOutOfBoundsFactory matrix =   
            let rows,cols = size matrix

            fun row col -> isOutOfBounds (rows,cols) (row,col)

        let printBase (matrix: char array2d) (iconRules : (Set<int*int>*char) seq) =
            let (rows,cols) = size matrix
            
            let _str = StringBuilder()
            let strn (text: string) = _str.AppendLine(text) |> ignore
            let str (text: string) = _str.Append(text) |> ignore

            let printColIndex () = 
                strn ""
                [|0..cols-1|] 
                |> Array.map (sprintf "%03d") 
                |> Array.map _.ToCharArray() 
                |> Array.transpose 
                |> Array.iter (fun digits-> 
                    str "   "
                    digits |> Array.iter((sprintf " %c")>>str)
                    strn ""
                )

                strn ""

            printColIndex()
            for row in 0..rows-1 do
                str $"%03d{row} "
                for col in 0..cols-1 do                    
                    iconRules 
                    |> Seq.tryFind (fun (cells,_) -> cells |> Set.contains (row,col))
                    |> function 
                    | Some (_,icon) -> icon
                    | None -> matrix.[row,col]
                    |> sprintf "%c " 
                    |> str
                str $"%03d{row}"
                if row < rows then strn ""
            printColIndex()
            strn ""
        
            printfn "%s" <| _str.ToString()