module Dec2024_03_CoPilot

open System.IO
open System.Text.RegularExpressions

// Function to extract valid mul(X,Y) instructions and calculate their results
let calculateSum (input: string) =
    let pattern = @"mul\((\d+),(\d+)\)"
    let matches = Regex.Matches(input, pattern)
    
    matches 
    |> Seq.cast<Match>
    |> Seq.map (fun m -> 
        let x = int (m.Groups.[1].Value)
        let y = int (m.Groups.[2].Value)
        x * y)
    |> Seq.sum

// Sample corrupted memory input
let input = 
    let filePath = $"""{__SOURCE_DIRECTORY__}/../2024/Dec2024_03.txt"""
    let reports = File.ReadAllText(filePath)
    reports

// Calculate the sum of valid mul instructions
let SolvePuzzleA() =
    
    let result = calculateSum(input)
    printfn "The sum of valid mul instructions is %d" result



(* PART TWO *)

open System.Text.RegularExpressions

// Function to extract valid mul(X,Y) instructions and calculate their results with conditional handling
let calculateSumWithConditions (input: string) =
    let pattern = @"(do\(\)|don't\(\)|mul\((\d+),(\d+)\))"
    let matches = Regex.Matches(input, pattern)
    
    let mutable enabled = true
    let sum =
        matches
        |> Seq.cast<Match>
        |> Seq.fold (fun acc m ->
            match m.Groups.[1].Value with
            | "do()" -> enabled <- true; acc
            | "don't()" -> enabled <- false; acc
            | _ when enabled -> 
                let x = int (m.Groups.[2].Value)
                let y = int (m.Groups.[3].Value)
                acc + (x * y)
            | _ -> acc) 0
    sum

let SolvePuzzleB() =
    // Calculate the sum of valid mul instructions with conditions
    let result = calculateSumWithConditions(input)
    printfn "The sum of valid mul instructions with conditions is %d" result
