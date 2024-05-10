module Dec2022_01A

open System

type ElfFood = int array
type ElfsFood = ElfFood array

let SolvePuzzle() : string =
    let parsedInputData = ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_01A.txt"""
    let normalizeInputData = 
        let normalizedLF = parsedInputData.ReplaceLineEndings("\n")
        normalizedLF.Split([|'\r';'\n'|], StringSplitOptions.TrimEntries)

    let rec ExtractGroups (nextGroup:ElfFood) (allGroups:ElfsFood) (foodItems:string array) : ElfsFood =
        match foodItems.Length with
        | 0 -> allGroups
        | _ ->
            let (valid, calories) = Int32.TryParse foodItems[0]
            match valid with 
            | true -> 
                let newNextGroup = Array.append nextGroup [|calories|]
                if foodItems.Length > 1 then
                    ExtractGroups newNextGroup allGroups foodItems[1..]
                else
                    Array.append allGroups [|newNextGroup|]
            | false ->
                let newAllGroups = Array.append allGroups [|nextGroup|]
                ExtractGroups [||] newAllGroups foodItems[1..]

    let FindTop3ElfsWithMostCalories (allElfs:ElfsFood) =
        
        let computeCaloriesPerElf (elfFood:ElfFood) = Array.sum elfFood

        allElfs 
            |> Array.mapi (fun i calories -> (i, computeCaloriesPerElf calories))
            |> Array.sortByDescending (fun (i, calories) -> calories)
            |> Array.take 3
        
    let top3ElfsWithMostCalories = 
        ExtractGroups [||] [||] normalizeInputData
        |> FindTop3ElfsWithMostCalories

    let output = 
        sprintf "Elf with most calories: %A\nTotal calories: %A" 
            top3ElfsWithMostCalories
            (top3ElfsWithMostCalories |> Array.sumBy (fun (i, calories) -> calories))
    output
