module Dec2022_02B

open System

type InMove = A = 1 | B = 2 | C = 3
type OutMove = X = 1 | Y = 2 | Z = 3
type GameOutcome = Lose = 0 | Draw = 3 | Win = 6

type Strategy = { Opponent: InMove; Player: OutMove }

let SolvePuzzle() =
    let parsedInputData = ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_02A.txt"""
    let normalizeInputData : Strategy array = 
        let ParseMoves (moves:string array) =
            let inMove = moves[0].ToUpperInvariant()
            let outMove = moves[1].ToUpperInvariant()
            match inMove with
            | "A" | "B" | "C" -> 
                let parsedInMove = InMove.Parse(inMove)
                match outMove with
                | "X" | "Y" | "Z" -> 
                    let parsedOutMove = OutMove.Parse(outMove)
                    let strategy = { Opponent = parsedInMove; Player = parsedOutMove }
                    strategy

                | _ -> failwith "Unknown strategy move"
            | _ -> failwith "Unknown opponent move"


        let normalizedLF = parsedInputData.ReplaceLineEndings("\n")
        normalizedLF.Split([|'\r';'\n'|], StringSplitOptions.TrimEntries)
        |> Array.map (fun strategy -> strategy.Split([|' '|]))
        |> Array.map ParseMoves

    let SelectAppropriatePlay (strategy:Strategy) =
        match (strategy.Opponent, strategy.Player) with
        | (InMove.A, OutMove.X) -> { Opponent = InMove.A; Player = OutMove.Z }
        | (InMove.A, OutMove.Y) -> { Opponent = InMove.A; Player = OutMove.X }
        | (InMove.A, OutMove.Z) -> { Opponent = InMove.A; Player = OutMove.Y }
        | (InMove.B, OutMove.X) -> { Opponent = InMove.B; Player = OutMove.X }
        | (InMove.B, OutMove.Y) -> { Opponent = InMove.B; Player = OutMove.Y }
        | (InMove.B, OutMove.Z) -> { Opponent = InMove.B; Player = OutMove.Z }
        | (InMove.C, OutMove.X) -> { Opponent = InMove.C; Player = OutMove.Y }
        | (InMove.C, OutMove.Y) -> { Opponent = InMove.C; Player = OutMove.Z }
        | (InMove.C, OutMove.Z) -> { Opponent = InMove.C; Player = OutMove.X }
        | _ -> failwith "Unknown strategy"

    let EvaluatePlay (strategy:Strategy) =
        match (strategy.Opponent, strategy.Player) with
        | (InMove.A, OutMove.X) -> (int)GameOutcome.Draw + (int)OutMove.X
        | (InMove.A, OutMove.Y) -> (int)GameOutcome.Win + (int)OutMove.Y
        | (InMove.A, OutMove.Z) -> (int)GameOutcome.Lose + (int)OutMove.Z
        | (InMove.B, OutMove.X) -> (int)GameOutcome.Lose + (int)OutMove.X
        | (InMove.B, OutMove.Y) -> (int)GameOutcome.Draw + (int)OutMove.Y
        | (InMove.B, OutMove.Z) -> (int)GameOutcome.Win + (int)OutMove.Z
        | (InMove.C, OutMove.X) -> (int)GameOutcome.Win + (int)OutMove.X
        | (InMove.C, OutMove.Y) -> (int)GameOutcome.Lose + (int)OutMove.Y
        | (InMove.C, OutMove.Z) -> (int)GameOutcome.Draw + (int)OutMove.Z
        | _ -> failwith "Unknown strategy"

    let output = 
        normalizeInputData
        |> Array.map (fun x -> SelectAppropriatePlay x)
        |> Array.map (fun x -> EvaluatePlay x)
        |> Array.sum 
    output |> printfn "%A"
