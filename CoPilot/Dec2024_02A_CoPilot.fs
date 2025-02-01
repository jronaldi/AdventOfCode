module Dec2024_02A_CoPilot

open System
open System.IO

let isIncreasing (levels: int list) =
    levels |> List.pairwise |> List.forall (fun (a, b) -> b > a && b - a <= 3)

let isDecreasing (levels: int list) =
    levels |> List.pairwise |> List.forall (fun (a, b) -> a > b && a - b <= 3)

let isSafe levels =
    isIncreasing levels || isDecreasing levels

let canBeMadeSafe (levels:int list) : bool =
    levels
    |> List.mapi (fun i _ -> (levels |> List.take i) @ (List.skip (i + 1) levels))
    |> List.exists isSafe

let processReport (report: string) =
    let levels = report.Split(' ') |> Array.map int |> List.ofArray
    isSafe levels || canBeMadeSafe levels

let countSafeReports (filePath: string) =
    let reports = File.ReadAllLines(filePath)
    reports |> Array.map processReport |> Array.filter id |> Array.length

let SolvePuzzleB() =
    //let filePath = $"""{__SOURCE_DIRECTORY__}/../2024/Dec2024_02A_Test.txt"""
    let filePath = $"""{__SOURCE_DIRECTORY__}/../2024/Dec2024_02A.txt"""
    let safeReportCount = countSafeReports filePath
    printfn "Number of safe reports: %d" safeReportCount
    0
