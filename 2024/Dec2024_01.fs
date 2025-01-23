module Dec2024_01

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

(* PUZZLE 1 *)

let normalizeInputData(filePath:string) = 

    let rawInputBlocks(filePath:string) = 
        ParseInputFile.ParseFile filePath
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))

    rawInputBlocks(filePath)

type LRList = int array
type Diff = { Diff:int array; }
type RawData = { SortedLeft:LRList; SortedRight:LRList; Score:LRList; }

let parseRawDataLine (rawLine:string):(int * int) =
    let l, r = ParsingHelpers.sscanf "%i %i" rawLine
    (l, r)

let extractLeft (data:int * int) : int = fst data
    
let extractRight (data:int * int) : int = snd data

let calculateDistance (x:int) (y:int) : int = Math.Abs(x - y)

let extractTwoListsWithDistance (stringData:string array) : RawData =

    let x1 = stringData |> Array.map (parseRawDataLine) 
    let rawLeft = x1 |> Array.map (extractLeft) |> Array.sort 
    let rawRight = x1 |> Array.map (extractRight) |> Array.sort
    let rawDist = Array.map2 (calculateDistance) rawLeft rawRight
    { SortedLeft=rawLeft; SortedRight=rawRight; Score=rawDist }

let calculateTotalScore (input:RawData) : int =
    input.Score |> Array.sum

let public SolvePuzzleA() =
        
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_01A_Test.txt""")
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_01A.txt""")
    |> extractTwoListsWithDistance
    |> calculateTotalScore


(* PUZZLE 2 *)

let calculateSimilarityScore (rightList:int array) (x:int) : int = 
    (* We need to compute 'x times how often it exists in rightList'. We take a shortcut by
       simply add 'x' each time it is found. *)
    Array.fold (fun (acc:int) (right:int) -> if right = x then acc+x else acc) 0 rightList

let extractTwoListsWithSimilarityScore (stringData:string array) : RawData =

    let x1 = stringData |> Array.map (parseRawDataLine) 
    let rawLeft = x1 |> Array.map (extractLeft) |> Array.sort 
    let rawRight = x1 |> Array.map (extractRight) |> Array.sort
    let rawSimScore = Array.map (calculateSimilarityScore rawRight) rawLeft
    { SortedLeft=rawLeft; SortedRight=rawRight; Score=rawSimScore }


let public SolvePuzzleB() =
    
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_01A_Test.txt""")
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_01A.txt""")
    |> extractTwoListsWithSimilarityScore
    |> calculateTotalScore
