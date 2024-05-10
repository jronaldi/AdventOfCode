module Dec2022_08

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

let normalizeInputData() = 

    let rawInputBlocks() = 
        ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_08.txt"""
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))
        |> Array.map (fun x -> x.ToCharArray() |> Array.map (fun y -> int(y - '0')))

    rawInputBlocks()

    
let public SolvePuzzleA() =
        
    normalizeInputData()

let public SolvePuzzleB() =
    
    normalizeInputData()

