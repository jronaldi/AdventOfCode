module Dec2022_04B

open System

type Range = { low:int; high:int }
type Pair = { pair1:Range; pair2:Range }

let SolvePuzzle() =
    let parsedInputData = ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_04.txt"""
    let normalizeInputData = 

        parsedInputData.ReplaceLineEndings("\n")
        |> (fun x -> x.Split([|'\r';'\n'|], StringSplitOptions.TrimEntries))
        |> Array.map (
            (fun (x:string) -> 
                let data =
                    x.Split([|'-';','|], StringSplitOptions.TrimEntries)
                    |> Array.map (fun x -> Int32.Parse(x))
                { pair1 = { low = data[0]; high = data[1] }; pair2 = { low = data[2]; high = data[3] } }
                ))

    let fullyEncompasses (data:Pair) =
        if data.pair1.high < data.pair2.low || data.pair1.low > data.pair2.high ||
           data.pair2.high < data.pair1.low || data.pair2.low > data.pair1.high 
           then 0
           else 1

    let output = normalizeInputData |> Seq.map fullyEncompasses |> Seq.sum

    output |> printfn "%A"
