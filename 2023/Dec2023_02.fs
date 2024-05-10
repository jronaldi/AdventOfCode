module Dec2023_02

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open System.Threading
open FParsec.CharParsers
open FParsec.Primitives

let GetNormalizedInputData(filePath:string) = 
    let ParseFile (filepath:string) =
        IO.File.ReadAllText (filepath, Text.Encoding.UTF8)

    let rawInputBlocks(filePath:string) = 
        ParseFile filePath
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))

    rawInputBlocks(filePath)

type GameSet = {
    r: int
    g: int
    b: int
}

type GameColor = RED | GREEN | BLUE

type Game = {
    id: int
    gameSet : GameSet list
}

let ParseGameData (allGames:string array) =
    let parseGameId = skipString "Game" >>. spaces >>. pint32 .>> spaces .>> skipString ":" .>> spaces
    
    let parseCubeColor : Parser<GameColor,unit> = 
        (stringReturn "red" RED)
        <|> (stringReturn "green" GREEN)
        <|> (stringReturn "blue" BLUE)

    let parseCubeData = spaces >>. pint32 .>> spaces .>>. parseCubeColor .>> spaces

    let parseAllCubes = spaces >>. sepBy parseCubeData (pchar ',') .>> spaces

    let parseGameEntry =
        parseGameId .>> spaces .>>. sepBy parseAllCubes (pchar ';')                  

    let convertToGameSet (rawGameSet : (int32 * GameColor) list) : GameSet =
         
        List.fold  
            (fun (state:GameSet) (rawCounts, color) ->
                match color with
                | RED -> { state with r=rawCounts }
                | GREEN -> { state with g=rawCounts }
                | BLUE -> { state with b=rawCounts }
            )
            { r=0; g=0; b=0; } rawGameSet

    let normalizeAllGames (rawParsedGames : ParserResult<(int32 * (int32 * GameColor) list list),unit>) = //(rawGameSet : (int32 * GameColor) list list) : GameSet list =
        match rawParsedGames with
        | ParserResult.Success ((gameId, rawGameSet),_,_) -> 
            rawGameSet 
            |> List.map
                (fun item -> 
                    convertToGameSet item
                )
            |> (fun gameSet -> { id=gameId; gameSet=gameSet })
        | ParserResult.Failure (errMsg,_,_) -> failwith errMsg

    let parseGameLine (line:string) = 
        line 
        |> run parseGameEntry
        |> normalizeAllGames

        //match result with
        //| Success(gameId,_,_) -> { id = gameId; gameSet = [] }
        //| Failure(errorMsg,_,_) -> 
        //    printfn "Parsing failed"
        //    { id = 0; gameSet = [] }

    allGames |> List.ofArray |> List.map parseGameLine 


let PossibleGames (bagContent:GameSet) (allGames : Game list)  =
    (
        allGames
        |> List.fold
            (fun validGame game ->
                game.gameSet 
                    |> List.exists 
                        (fun gameSet -> 
                            gameSet.r > bagContent.r ||
                            gameSet.g > bagContent.g ||
                            gameSet.b > bagContent.b
                        )
                    |> (fun illegal -> 
                        match illegal with 
                        |true -> validGame 
                        |false -> validGame+game.id)
            )
            0
    )


let MinimalGames (allGames : Game list) : int =
    (
        // The smallest number of cubes that makes the game possible will be the largest
        // number of cubes pulled out in a game.
        let findSmallestCubeCounts (gameSets: GameSet list) : GameSet =
            gameSets |>
                List.fold<GameSet, GameSet> 
                    (fun minCubes gameSet -> 
                        let r = if gameSet.r > minCubes.r then gameSet.r else minCubes.r;
                        let g = if gameSet.g > minCubes.g then gameSet.g else minCubes.g;
                        let b = if gameSet.b > minCubes.b then gameSet.b else minCubes.b; 
                        { r=r; g=g; b=b; }
                    )
                    { r=0; g=0; b=0; }

        allGames
        |> List.fold
            (fun cubePower game ->
                game.gameSet 
                    |> findSmallestCubeCounts
                    |> (fun minGames ->
                            //printfn 
                            //    "%A + %A * %A * %A = %A" 
                            //    cubePower minGames.r minGames.g minGames.b
                            //    (cubePower + minGames.r * minGames.g * minGames.b)

                            cubePower + (minGames.r * minGames.g * minGames.b)
                        )
            )
            0
    )

    
let public SolvePuzzleA() =
        
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_02A_Test.txt""")
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_02A.txt""")
    |> ParseGameData
    |> PossibleGames { r=12; g=13; b=14; }



let public SolvePuzzleB() =
    
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_02A_Test.txt""")
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_02A.txt""")
    |> ParseGameData
    |> MinimalGames 


SolvePuzzleA()
SolvePuzzleB()
