module Day20

open System.Collections.Generic
open System.Text.RegularExpressions

type Pulse = High | Low

type FlipFlop = {
    mutable On: bool
    Dests: string list }

type Conjunction = {
    mutable Inputs: Map<string, Pulse>
    Dests: string list }

type Broadcaster = {
    Dests: string list }

type Module =
    | FlipFlop of FlipFlop
    | Conjunction of Conjunction
    | Broadcaster of Broadcaster
    | Output

type Modules = Map<string, Module>
type Message = (string * Pulse * string)

type MessageQueue = {
    Queue: Queue<Message>
    mutable HighCount: int64
    mutable LowCount: int64 }

type Stats = {
    mutable ButtonCount: int64
    mutable SxPeriod: int64 option
    mutable JtPeriod: int64 option
    mutable KbPeriod: int64 option
    mutable KsPeriod: int64 option }

type State = {
    MessageQueue: MessageQueue
    Stats: Stats }

let parseLines (lines: string list) =
    let parseLine (line: string) =
        let [| modTxt; destTxt |] = Regex.Split(line, " -> ")
        let kind, name =
            match modTxt[0], modTxt[1..] with
            | '%', name -> ('%', name)
            | '&', name -> ('&', name)
            | 'b', "roadcaster" -> 'b', modTxt
        let destinations = Regex.Split(destTxt, ", ") |> List.ofArray
        ((kind, name), destinations)
    lines |> List.map parseLine

let rec enqueue (queue: MessageQueue) (messages: Message list) =
    match messages with
    | [] -> ()
    | m::ms ->
        queue.Queue.Enqueue m
        enqueue queue ms

let dequeue (queue: MessageQueue) =
    match queue.Queue.TryDequeue () with
    | (true, ((_, pulse, _) as msg)) ->
        match pulse with
        | High -> queue.HighCount <- queue.HighCount + 1L
        | Low -> queue.LowCount <- queue.LowCount + 1L
        Some msg
    | (false, _) -> None

let constructModules specs =
    let sendersTo dest =
        specs
        |> List.filter (fun (_, dests) -> dests |> List.contains dest)
        |> List.map (fst >> snd)

    let flipFlops =
        specs
        |> List.filter (fun ((kind, _), _) -> kind = '%')
        |> List.map (fun ((_, name), dests) ->
            (name, FlipFlop { On = false; Dests = dests}))
    let conjunctions =
        specs
        |> List.filter (fun ((kind, _), _) -> kind = '&')
        |> List.map (fun ((_, name), dests) ->
            let inputs =
                sendersTo name
                |> List.map (fun sndr -> (sndr, Low))
                |> Map
            (name, Conjunction {Inputs = inputs; Dests = dests}))
    let broadcasters =
        specs
        |> List.filter (fun ((kind, _), _) -> kind = 'b')
        |> List.map (fun ((_, name), dests) ->
            (name,  Broadcaster {(*Input = Input ();*) Dests = dests}))

    [broadcasters; flipFlops; conjunctions]
    |> List.collect id
    |> Map
    |> Map.add "output" Output

let send (modules: Modules) ((source, pulse, dest) as msg: Message) stats =

    if dest = "zh" && pulse = High then
        let period prev curr =
            match prev with | None -> Some curr | Some _ -> prev
        let butCount = stats.ButtonCount
        if source = "sx" then stats.SxPeriod <- period stats.SxPeriod butCount
        elif source = "jt" then stats.JtPeriod <- period stats.JtPeriod butCount
        elif source = "kb" then stats.KbPeriod <- period stats.KbPeriod butCount
        elif source = "ks" then stats.KsPeriod <- period stats.KsPeriod butCount

    let newMessages dests outPulse =
        dests |> List.map (fun next -> (dest, outPulse, next))

    let sendToFlipFlop (ff: FlipFlop) (_, pulse, _) =
        match pulse with
        | High -> []
        | Low ->
            ff.On <- not ff.On
            match ff.On with
            | true -> High
            | false -> Low
            |> newMessages ff.Dests

    let sendToConjunction (cn: Conjunction) (source, pulse, _) =
        cn.Inputs <- cn.Inputs |> Map.add source pulse
        match cn.Inputs |> Map.toList |> List.forall (snd >> ((=) High)) with
        | true -> Low
        | false -> High
        |> newMessages cn.Dests

    let sendToBroadcaster (br: Broadcaster) (_, pulse, _) =
        newMessages br.Dests pulse

    if dest = "rx" then
        []
    else
        match modules[dest] with
        | FlipFlop ff -> sendToFlipFlop ff msg
        | Conjunction cn -> sendToConjunction cn msg
        | Broadcaster br -> sendToBroadcaster br msg
        | Output -> []

let rec processMessages (mods: Modules) (state: State) =
    match dequeue state.MessageQueue with
    | None -> state
    | Some msg  ->
        send mods msg (state.Stats) |> enqueue state.MessageQueue
        processMessages mods state

let pressButton (modules: Modules) (state: State) =
    enqueue state.MessageQueue [("button", Low, "broadcaster")]
    state.Stats.ButtonCount <- 1L + state.Stats.ButtonCount
    processMessages modules state

let newState () = {
    MessageQueue = {
        Queue = Queue<Message> ()
        HighCount = 0
        LowCount = 0 }
    Stats = {
        ButtonCount = 0L
        SxPeriod = None
        JtPeriod = None
        KbPeriod = None
        KsPeriod = None } }

let rec repeat func n x = if n = 0 then x else repeat func (n - 1) (func x)
let result (state: State) =
    printfn "hi: %d lo: %d" state.MessageQueue.HighCount state.MessageQueue.LowCount
    state.MessageQueue.HighCount * state.MessageQueue.LowCount

let part1() = // (getLines: string -> string list) =
    let mods = 
        System.IO.File.ReadAllLines(@"C:\Users\cernu\source\advent-of-code-2023\day20\console\input\puzzle.input")
        |> List.ofArray |> parseLines |> constructModules
    //let mods = "input" |> getLines  
    newState () |> repeat (pressButton mods) 1_000 |> result

(*  We require: low -> rx
                =>  &zh -> low
                =>  &zh remembers high for all inputs
                =>  &sx -> high
                    &jt -> high
                    &kb -> high
                    &ks -> high

    A bit of printf'ng reveals these four only go high once every p presses
    where p is different four-digit prime for each of sx, jt, kb and ks.      *)

let part2 (getLines: string -> string list) =
    let mods = "input" |> getLines |> parseLines |> constructModules
    let rec pressAndCheck mods state =
        let stats = state.Stats
        let periods =
            [stats.SxPeriod; stats.JtPeriod; stats.KbPeriod; stats.KsPeriod]
        if periods |> List.exists ((=) None) then
            pressAndCheck mods (pressButton mods state)
        else
            periods |> List.map Option.get |> List.reduce (*)
    pressAndCheck mods (newState())