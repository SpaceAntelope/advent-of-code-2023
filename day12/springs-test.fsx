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





// let permutations(mask: string) (crc: int[]) =
//     let maxDots = mask.Length - Array.sum crc
//     let dotIslandCount = crc.Length + 2 //max depth. first and last island can have zero length
    
//     let rec search (min:int) (max:int) (islands: int list) = 
//         let dotsUsed = islands |> List.sum
//         let remainingIslands = 
//         let dotsNedeedToSeparateRemainingIslands = crc.Length - 1 - (depth - 1)
//         for i in [min..max] do 
//             search min (maxDots - dotsUsed - dotsNedeedToSeparateRemainingIslands) (depth+1) (dotsUsed + i)

//     search 0 (maxDots - )

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
        printfn $"{mask} %A{crc} {expected}"
        match permutations mask crc with
        | actual when actual |> Seq.length = expected -> printfn $"Passed arrangement count test for {mask}. Arrangements were %A{actual}"
        | actual -> failwith $"Arrangement count test failed: expected {expected} but got {actual |> Seq.length}" )