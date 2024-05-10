module Dec2022_06

open System
open System.Text.RegularExpressions

let normalizeInputData() = 

    let rawInputBlocks = 
        ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_06.txt"""
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))
        |> List.ofArray
    rawInputBlocks
    
let solve (packet:string) (markerLength:int) =

    let rec existsDuplicate (start:int) (length:int) =
        if length <= 1 then false
        else
            let searchFor = packet[start]
            seq { start+1..start+length-1 }
                |> (fun allIndexes -> 
                    if Seq.exists (fun j -> packet[j] = searchFor) allIndexes then true
                    else existsDuplicate (start+1) (length-1))
    
    seq { 0..packet.Length-markerLength }
        |> Seq.pick (fun i -> if existsDuplicate i markerLength then None else Some (i+markerLength))

let public SolvePuzzleA() =
               
    normalizeInputData()
    |> List.map (fun x -> solve x 4)

let public SolvePuzzleB() = 

    normalizeInputData()
    |> List.map (fun x -> solve x 14)
