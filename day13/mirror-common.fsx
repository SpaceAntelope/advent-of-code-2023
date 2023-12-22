open System

let invert (table: 'a array2d) = 
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2
    Array2D.init colCount rowCount (fun row col -> table[col,row])


let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.ToCharArray()
    |> array2D

let reverseColumns (table: 'a array2d) =
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2

    Array2D.init rowCount colCount (fun row col -> table[row,^col])
let reverseRows (table: 'a array2d) =
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2

    Array2D.init rowCount colCount (fun row col -> table[^row,col])

let rowSymAt r1 r2 (table: 'a array2d) =
    let rowCount = table |> Array2D.length1
    table[r1,*] = table[r2,*] 
    && ((r1 < rowCount-r2 && table[..r1, *] = reverseRows table[r2..r2+r1, *])
        || table[r1-(rowCount-r2-1)..r1, *] = reverseRows table[r2..,*])
    

let colSymAt c1 c2 (table: 'a array2d) =
    let colCount = table |> Array2D.length2
    table[*,c1] = table[*,c2]
    && ( (c1 < colCount-c2 && table[*,..c1] = reverseColumns table[*, c2..c2+c1])
         || table[*, c1-(colCount-c2-1)..c1] = (reverseColumns table[*,c2..]))             

let symScore (table: char array2d) =
    // printfn "%A" table
    
    let rowCount = table |> Array2D.length1
    let colCount = table |> Array2D.length2

    let vScore = 
        [|0..rowCount-1|]
        |> Array.pairwise
        |> Array.tryFind (fun (r1,r2) -> rowSymAt r1 r2 table)            
        |> function
        | Some (r1,r2) -> 
            // printfn $"V EQ:\t%d{r1} %A{table[r1,*]}\n\t%A{table[r2,*]}";
            (r1+1) * 100
        | None -> 0
        

    let hScore = 
        [|0..colCount-1|]
        |> Array.pairwise
        |> Array.tryFind (fun (c1,c2) -> colSymAt c1 c2 table)
        |> function
        | Some (c1,c2) -> 
            // printfn $"H EQ: %d{c1}\t%A{table[*,c1]}\n\t%A{table[*,c2]}";  
            (c1+1) 
        | None -> 0
        
    vScore + hScore

