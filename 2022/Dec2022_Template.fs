module Dec2022_Template

open System

let SolvePuzzle() =
    let parsedInputData = ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_02A.txt"""
    let normalizeInputData = 

        let normalizedLF = parsedInputData.ReplaceLineEndings("\n")
        normalizedLF.Split([|'\r';'\n'|], StringSplitOptions.TrimEntries)
        |> Array.map (fun strategy -> strategy.Split([|' '|]))

    let output = normalizeInputData
    output |> printfn "%A"
