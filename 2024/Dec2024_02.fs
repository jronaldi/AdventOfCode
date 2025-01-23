module Dec2024_02

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

(* PUZZLE 1 *)

let normalizeInputData(filePath:string) = 

    let rawInputBlocks(filePath:string) = 
        ParseInputFile.ParseFile filePath
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))

    rawInputBlocks(filePath)

type LevelsDirection = UNKNOWN | UP | DOWN

type RulesState = {
    initialized : bool;
    lastLevel : int;
    direction : LevelsDirection;
    valid : bool;
    }

let parseRawDataLine (rawLine:string):int array =
    let rawLevels = rawLine.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let levels = rawLevels |> Array.map (fun x -> Int32.Parse x)
    levels

let validateLevelRules (levels:int array) : bool =
    let isInvalidLevel (state:RulesState) (level:int) : RulesState =
        match state.initialized with
        | false -> 
            { RulesState.initialized = true; lastLevel = level; direction=UNKNOWN; valid = true; }
        | true ->
            match state.valid with
            | false -> state
            | true -> 
                let newDifference = Math.Abs(level - state.lastLevel)
                let newDirection = 
                    if (level > state.lastLevel) then LevelsDirection.UP
                    elif (level < state.lastLevel) then LevelsDirection.DOWN
                    else LevelsDirection.UNKNOWN
                let newValid = 
                    if newDifference > 0 && newDifference <= 3 
                        && (state.direction = UNKNOWN 
                        || state.direction = newDirection) then true else false
                
                { state with lastLevel = level; direction = newDirection; valid = newValid; }

    let validationResult = 
        levels 
        |> Array.fold isInvalidLevel { RulesState.initialized = false; lastLevel = 0; direction=UNKNOWN; valid = true; } 
    validationResult.valid

let countValidReports (levels:int array) : int =
    if validateLevelRules levels = true then 1 else 0

let public SolvePuzzleA() =
        
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_02A_Test.txt""")
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_02A.txt""")
    |> Array.map parseRawDataLine
    |> Array.map countValidReports
    |> Array.sum

(* PUZZLE 2 *)



let public SolvePuzzleB() =
    
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_02A_Test.txt""")
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2024_02A.txt""")
    |> Array.map parseRawDataLine
