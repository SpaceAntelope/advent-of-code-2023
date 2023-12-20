#load "./springs-common.fsx"

open System
open System.Text.RegularExpressions
open ``Springs-common``

let testData = @"
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"


let testValidation =
    let test mask crc spring  = 
        if validatebyCRC crc spring 
        then printfn $"Validation {spring} by crc %A{crc} passed" 
        else failwith $"Validation of {spring} by crc %A{crc} failed"

        if validateByMask mask spring 
        then printfn $"Validation {spring} by mask {mask} passed" 
        else failwith $"Validation of {spring} by mask {mask} failed"
        
        let partialSpring = spring.Substring(0, spring.Length/2)
        if validateByMask mask partialSpring
        then printfn $"Partial validation {partialSpring} by mask {mask} passed" 
        else failwith $"Partial validation of {partialSpring} by mask {mask} failed"

    @"#.#.### ???.### 1,1,3
.#...#....###. .??..??...?##. 1,1,3
.#.###.#.###### ?#?#?#?#?#?#?#? 1,3,1,6
####.#...#... ????.#...#... 4,1,1
#....######..#####. ????.######..#####. 1,6,5
.###.##....# ?###???????? 3,2,1".Split('\n')
    |> Array.map (fun line -> 
        let parts = line.Split(' ')
        parts[0], parts[1], parts[2].Split(',') |> Array.map int)
    |> Array.iter (fun (spring, mask, crc) -> test mask crc spring )
    



let testArrangementGenerationFromIndices = 
    let test (expected: string) =
        let indices,crc =
            Regex.Matches(expected, "#+") 
            |> Seq.cast<Match> 
            |> Seq.map (fun m -> m.Index, m.Length)
            |> Array.ofSeq
            |> Array.unzip
            
        let actual = generateArrangementFromIslandIndices indices crc expected.Length
        if actual = expected
        then printfn $"Arrangement generation test passed for %A{expected}"
        else failwith $"Arrangement generation test failed: expected [{expected}] but got [{actual}]"

    @"#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1".Split('\n') 
    |> Array.map (_.Split(' ')>>Array.head)
    |> Array.iter test    




let testPermutations = 
    @"???.### 1,1,3 - 1 arrangement
.??..??...?##. 1,1,3 - 4 arrangements
?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
????.#...#... 4,1,1 - 1 arrangement
????.######..#####. 1,6,5 - 4 arrangements
?###???????? 3,2,1 - 10 arrangements".Split('\n')
    |> Array.map (fun line -> 
        let parts = line.Split(' ')
        parts[0], parts[1].Split(',') |> Array.map int, int parts[3])
    |> Array.iter (fun (mask, crc, expected) -> 
        match permutations mask crc with
        | actual when expected = Seq.length actual -> printfn $"Passed arrangement count test for {mask}. Found {expected} arrangements: %A{actual}"
        | actual -> failwith $"Arrangement count test for %A{mask} failed: expected {expected} but got {actual |> Seq.length}" )

let testPermutationCountTotal = 
    let expected = 21
    @"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"
    |> parse
    |> Array.map (fun spring -> 
        permutations spring.Mask spring.CRC)
    |> Array.sumBy Seq.length
    |> function
    | actual when expected = actual -> printfn "Permutation count for dataset test passed."
    | actual -> printfn $"Permutation count for dataset test failed. Expected {expected} but fot {actual}."

let testUnfoldedPermutations =
    let expectedTotal = 525152L
    
    @"???.### 1,1,3 - 1 arrangement
.??..??...?##. 1,1,3 - 16384 arrangements
?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
????.#...#... 4,1,1 - 16 arrangements
????.######..#####. 1,6,5 - 2500 arrangements
?###???????? 3,2,1 - 506250 arrangements".Split('\n')
    |> Array.map (fun line -> 
        let parts = line.Split(' ') |> Array.map _.Trim()
        { Mask = parts[0]; CRC = parts[1].Split(',') |> Array.map int }, 
        int <| parts[3].Split(' ')[0])
    |> Array.map (fun (spring, expected) -> 
        let unfolded = unfold spring
        let arrangements = permutations unfolded.Mask unfolded.CRC |> Array.ofSeq
        match arrangements with
        | actual when expected = actual.Length -> printfn $"Passed unfolded arrangement count test for %A{unfolded}."; actual.LongLength
        | actual -> failwith $"Unfolded arrangement count test for %A{unfolded} failed: expected {expected} but got {actual.Length}" )
    |> Array.sum
    |> function
    | actual when actual = expectedTotal -> printfn "Unfolded arrangement total count passed"
    | actual -> printfn $"Unfolded arrangement total count failed: Expected {expectedTotal} but got {actual}"


// let testEdgeCase = 
//     { Mask = "????.??.??.???"; CRC = [|1;2|]
    
//     }    