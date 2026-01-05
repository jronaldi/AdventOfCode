module Dec2025_Template

open System
open Microsoft.FSharp.Collections

(* PUZZLE 1 *)

let normalizeInputData(filePath:string) = 

    let rawInputBlocks(filePath:string) = 
        ParseInputFile.ParseFile filePath
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))

    rawInputBlocks(filePath)

let public SolvePuzzleA() =
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2025_01_Test.txt""")
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2025_01_Input.txt""")

let public SolvePuzzleB() =
    
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2025_01B_Test.txt""")
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2025_01B_Input.txt""")
    |> ignore
    "Not implemented yet"

let public SolvePuzzle() =
    let resultA = SolvePuzzleA()
    printfn "Puzzle 2025-01A: %A" resultA   
    let resultB = SolvePuzzleB()
    printfn "Puzzle 2025-01B: %A" resultB  
