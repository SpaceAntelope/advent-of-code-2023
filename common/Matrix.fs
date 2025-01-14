namespace Common

    module Matrix =
        open System.Text.RegularExpressions

        let allIndices matrix =
                let rows = matrix |> Array2D.length1
                let cols = matrix |> Array2D.length2
                ([0..rows-1],[0..cols-1])
                ||> Seq.allPairs

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

        let isOutOfBounds (size: int*int) (point: int*int) =
                let rows,cols = size
                let row,col = point

                row > rows-1 || col > cols-1 || col < 0 || row < 0
        
        let isOutOfBoundsFactory matrix =   
            let rows,cols = size matrix

            fun row col -> isOutOfBounds (rows,cols) (row,col)


        let printBase (matrix: char array2d) (iconRules : (Set<int*int>*char) seq) =
            let (rows,cols) = size matrix
            
            let printColIndex () = 
                [|0..cols-1|] 
                |> Array.map (sprintf "%03d") 
                |> Array.map _.ToCharArray() 
                |> Array.transpose 
                |> Array.iter (fun digits-> 
                    printf "   "
                    digits |> Array.iter(printf " %c")
                    printfn ""
                )

            printColIndex()
            for row in 0..rows-1 do
                printf "%03d " row
                for col in 0..cols-1 do                    
                    iconRules 
                    |> Seq.tryFind (fun (cells,_) -> cells |> Set.contains (row,col))
                    |> function 
                    | Some (_,icon) -> icon
                    | None -> matrix.[row,col]
                    |> printf "%c "                   
                printf "%03d" row
                if row < rows then printfn ""
            printColIndex()
            printfn ""
        
            