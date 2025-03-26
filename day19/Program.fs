namespace Day19

module Main =
    open System
    open System.IO
    open System.Text.RegularExpressions

    type MachinePart = {
        x: int64
        m: int64
        a: int64
        s: int64
    } with static member FromString (str: string) = 
            let matches = Regex.Matches(str, @"(\w)=(\d+)")
            {
                x = matches.[0].Groups.[2].Value |> Convert.ToInt64
                m = matches.[1].Groups.[2].Value |> Convert.ToInt64
                a = matches.[2].Groups.[2].Value |> Convert.ToInt64
                s = matches.[3].Groups.[2].Value |> Convert.ToInt64
            }

            member x.TotalRating() = x.x + x.m + x.a + x.s

    type Rating = X | M | A | S with static member FromString (str: string) = match str with "a" -> A | "m" -> M | "x" -> X | "s" -> S | x -> failwith $"Invalid rating type {x}."
    type Operator = Lt | Gt with static member FromString (str: string) = match str with "<" -> Lt | ">" -> Gt | x -> failwith $"Invalid operator {x}."    
    type Target = Key of string | Accept | Reject with static member FromString (str: string) = match str with "R" -> Reject | "A" -> Accept | key -> Key key

    type Rule = {
        Rating: Rating
        Operator: Operator
        Value: int64
        Target: Target
    } with 
        member x.Evaluate(part: MachinePart) =
            let ratingValue = 
                match x.Rating with
                | X -> part.x
                | M -> part.m
                | A -> part.a
                | S -> part.s

            match x.Operator with
            | Lt -> ratingValue < x.Value
            | Gt -> ratingValue > x.Value

        static member FromString (str: string) = 
                let matches = Regex.Match(str, @"(\w)([<>])(\d+):(\w+)")
                let rating = Rating.FromString matches.Groups[1].Value
                let op = Operator.FromString matches.Groups[2].Value
                let value = Convert.ToInt64 matches.Groups[3].Value
                let key = Target.FromString matches.Groups[4].Value
                { 
                    Rating = rating
                    Operator = op
                    Value = value
                    Target = key
                }

    
    type Workflow = {
        Key: string
        Rules: Rule[]
        LastRule: Target
    } with 
        static member FromLine (line: string) = 
                let matches = Regex.Match(line, "(^\\w+)\\{([^\\}]+)\\}")
                let key = matches.Groups.[1].Value
                let rules = matches.Groups.[2].Value.Split(',')
                let lastRule = rules |> Array.last |> Target.FromString
                let redirectRules = rules |> Array.take (rules.Length - 1) |> Array.map Rule.FromString
                {
                    Key = key
                    Rules = redirectRules
                    LastRule = lastRule
                }

    type ProcessedPart = Accepted of MachinePart | Rejected of MachinePart

    let parse (path: string) =
        let lines  = path |> File.ReadAllLines
        let workflows = 
            lines 
            |> Array.takeWhile (String.IsNullOrEmpty>>not)
            |> Array.map Workflow.FromLine

        let ratings= 
            lines 
            |> Array.skipWhile (fun line -> String.IsNullOrEmpty(line) || line.[0] <> '{')
            |> Array.map MachinePart.FromString

        ratings, workflows

    let Part1() = 

        let applyWorkflows (workflows: Workflow[]) (part: MachinePart)=
            let index = workflows |> Array.map (fun wf -> wf.Key, wf) |> readOnlyDict

            let rec apply (wf : Workflow) = 
                wf.Rules 
                |> Array.tryFind _.Evaluate(part)
                |> function
                | Some rule -> rule.Target
                | None -> wf.LastRule 
                |> function
                | Accept -> Accepted part
                | Reject -> Rejected part
                | Key key -> apply (index.[key])

            apply index.["in"]
        
        let sumAcceptedRatings (path:string) =
            path
            |> parse 
            |> fun (ratings, workflows) ->
                    ratings
                    |> Array.map (applyWorkflows workflows)
            |> Array.sumBy (fun pp -> 
                match pp with
                | Accepted part -> part.TotalRating()
                | _ -> 0L)
            
        "./input/puzzle.example"
        |> sumAcceptedRatings
        |> Common.Assertions.shouldBe 19114
        
        "./input/puzzle.input"
        |> sumAcceptedRatings
        |> printfn "If you add together all of the rating numbers for all of the parts that ultimately get accepted you get %d"

    Part1()