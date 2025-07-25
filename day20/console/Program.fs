namespace Day20

open Model

module Program = 
    open System.Reflection
    open System.IO
    open System.Text

    let part1() = 
        let inputPath = Path.Join(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "./input/puzzle.input")
        let network, _ = Day20.Part1.parse inputPath
        
        let log = new StringBuilder()
        for i in 1..5 do 
            log.AppendLine($"\r\nButton push #{i}") |> ignore
            Day20.Part1.processPulses (Some log) network            
            printfn "%O" log


        let evaluation = Day20.Part1.evaluate network

        printfn "If you multiply the total number of low pulses sent by the total number of high pulses sent you get %A" evaluation
    

    [<EntryPoint>]
    let main argv =
        

        part1()

        1