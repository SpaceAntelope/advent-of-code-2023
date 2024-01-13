open System

let testData =
    @"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"

type Direction =
    | D
    | U
    | L
    | R

let DirIndex = [ 'D', D; 'U', U; 'L', L; 'R', R ] |> readOnlyDict

type Instruction =
    { Dir: Direction
      Steps: int
      Color: string }

let parse (data: string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map (fun line ->
        let parts = line.Split(' ')

        { Dir = DirIndex[parts[0][0]]
          Steps = int parts[1]
          Color = parts[2].Trim([| '('; ')'; '#' |]) })

let path (instructions: Instruction[]) =
    instructions
    |> Array.scan (fun state instr -> 
        let (row,col) = state
        let k = 
            [|1..instr.Steps|]
            |> Array.map (fun i -> 
                    match instr.Dir with
                    | D ->  (row+i, col)
                    | U -> (row-i, col)
                    | R -> (row, col+i)
                    | R -> (row, col-i))
        k) [|(0,0)|]

[1..10] |> List.scan ( fun state current -> state + current) 0