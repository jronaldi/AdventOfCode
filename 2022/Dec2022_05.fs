module Dec2022_05

open System
open System.Text.RegularExpressions

type ReorderingCommand = {
    Count: int
    Source: int
    Destination: int
    }

let normalizeInputData() = 

    let parseSetupBlock (rows:string list) =
        rows 
        |> List.map 
            (fun line -> 
                line.Split([|"    "; "   "; "  "; " "|], StringSplitOptions.TrimEntries)
                |> List.ofArray)
        |> List.transpose
        |> List.map 
            (fun stack ->
                stack 
                |> List.choose 
                    (fun container -> if String.IsNullOrEmpty(container) then None else Some container))
        |> Array.ofList

    let parseReorderingBlock (rows:string list) =
        rows 
            |> List.map 
            (fun line -> 
                let parsedLine = Regex.Match (line, """move (\d+) from (\d+) to (\d+)""")
                { 
                    Count=parsedLine.Groups[1].Value |> int
                    Source=parsedLine.Groups[2].Value |> int
                    Destination=parsedLine.Groups[3].Value |> int
                }
                )

    let rawInputBlocks = 
        ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_05.txt"""
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))
        |> List.ofArray
        |> (fun inputLines ->
                let splitPosition = inputLines |> List.findIndex (fun line -> line |> String.IsNullOrEmpty) 
                let initialSetupBlock = inputLines.GetSlice (None, Some (splitPosition-2))
                let reorderingCommandsBlock = inputLines.GetSlice (Some (splitPosition+1), None)
                (initialSetupBlock, reorderingCommandsBlock)
            )

    let (initialSetupBlock, reorderingCommandsBlock) = rawInputBlocks
    (initialSetupBlock |> parseSetupBlock,
     reorderingCommandsBlock |> parseReorderingBlock)
    
let getTopOfStacks (stacks:string list array) =
    stacks |> Array.map (fun stack -> if stack.IsEmpty then "" else stack.Head)


let public SolvePuzzleA() =
    
    let executeMoves (stacks:string list array) (command:ReorderingCommand) =
        seq {1..command.Count} 
            |> Seq.fold (
            fun (stacks:string list array) _ -> 
                let sourceIndex = command.Source - 1
                let container = stacks[sourceIndex].Head
                stacks.SetValue (stacks[sourceIndex].Tail, sourceIndex)
                let destIndex = command.Destination - 1
                let newStack = container :: stacks[destIndex]
                stacks.SetValue (newStack, destIndex)
                stacks
                ) stacks 

    let executeMovesMultiple (stacks:string list array) (command:ReorderingCommand) =
        let sourceIndex = command.Source - 1
        let containers = stacks[sourceIndex].GetSlice (None, Some (command.Count-1))
        stacks.SetValue (stacks[sourceIndex].GetSlice (Some command.Count, None), sourceIndex)
        let destIndex = command.Destination - 1
        let newStack = containers @ stacks[destIndex]
        stacks.SetValue (newStack, destIndex)
        stacks

    let (setup, commands) = normalizeInputData()  
    commands 
        |> List.fold (fun stacks command -> executeMoves setup command) setup 
        |> getTopOfStacks

let public SolvePuzzleB() = 

    let executeMovesMultiple (stacks:string list array) (command:ReorderingCommand) =
        let sourceIndex = command.Source - 1
        let containers = stacks[sourceIndex].GetSlice (None, Some (command.Count-1))
        stacks.SetValue (stacks[sourceIndex].GetSlice (Some command.Count, None), sourceIndex)
        let destIndex = command.Destination - 1
        let newStack = containers @ stacks[destIndex]
        stacks.SetValue (newStack, destIndex)
        stacks

    let (setup, commands) = normalizeInputData()  
    commands 
        |> List.fold (fun stacks command -> executeMovesMultiple setup command) setup 
        |> getTopOfStacks

