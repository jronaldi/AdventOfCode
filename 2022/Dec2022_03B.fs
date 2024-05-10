module Dec2022_03B

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

        let findCommonPriorityItem (items:(int * string) array) =
            let second = items[1] |> snd
            let third = items[2] |> snd

            items[0]
            |> snd
            |> (fun x -> x.ToCharArray())
            |> Array.find (fun x -> second.Contains(x) && third.Contains(x))

        normalizedLF.Split([|'\r';'\n'|], StringSplitOptions.TrimEntries)
        // Create groups of 3
        |> Array.mapi (fun i x -> (i, x))
        |> Array.groupBy (fun (i, x) -> i / 3)
        |> Array.map (fun x -> snd x)

        // Find common priority item
        |> Array.map findCommonPriorityItem

        |> Seq.map computePriority 
        |> Seq.sum

        //|> Seq.map (fun x -> 
        //    let containerLength = x.Length / 2
        //    seq { 
        //        let container1 = x.AsSpan(0, containerLength)
        //        let container2 = x.AsSpan(containerLength)
        //        for c in container1.ToArray() do
        //            if x.LastIndexOf(c) >= containerLength then yield c 
        //    }
        //    |> Seq.distinct
        //    )
        //|> Seq.map computePriority 
        //|> Seq.sum

    let output = normalizeInputData
    output |> printfn "%A"
