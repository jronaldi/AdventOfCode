module Dec2022_03A

open System
open System.Text

let SolvePuzzle() =
    let parsedInputData = ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_03A.txt"""

    let computePriority (item:char) =
        match Char.IsAsciiLetterLower(item) with
        | true -> (int)(item - 'a') + 1
        | false -> (int)(item - 'A') + 27

    let normalizeInputData = 
        let normalizedLF = parsedInputData.ReplaceLineEndings("\n")

        normalizedLF.Split([|'\r';'\n'|], StringSplitOptions.TrimEntries)
        |> Seq.collect (fun x -> 
            let containerLength = x.Length / 2
            seq { 
                let container1 = x.AsSpan(0, containerLength)
                let container2 = x.AsSpan(containerLength)
                for c in container1.ToArray() do
                    if x.LastIndexOf(c) >= containerLength then yield c 
            }
            |> Seq.distinct
            )
        |> Seq.map computePriority 
        |> Seq.sum

    let output = normalizeInputData
    output |> printfn "%A"
