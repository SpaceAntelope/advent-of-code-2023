namespace Day20

open Model

module Program =
    open System.Reflection
    open System.IO
    open System.Text

    let part1 () =
        let inputPath =
            Path.Join(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "./input/puzzle.input")

        let network, _ = Day20.Part1.parse inputPath
        let iterations = 1000
        let sb = StringBuilder()
        let log : string -> unit = 
            if iterations < 15 then
                fun msg -> sb.AppendLine(msg) |> ignore
            else
                fun msg -> ()

        for i in 1 .. 1000 do

            Day20.Part1.processPulses log network

            if sb.Length > 0 then printfn "%O" sb


        let evaluation = Day20.Part1.evaluate network

        printfn
            "If you multiply the total number of low pulses sent by the total number of high pulses sent you get %A"
            evaluation


    [<EntryPoint>]
    let main argv =


        part1 ()

        1
