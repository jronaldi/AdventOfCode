module Dec2023_04 // Problem link: https://adventofcode.com/2023/day/4

open System
open FParsec

let GetNormalizedInputData(filePath:string) = 
    let ParseFile (filepath:string) =
        printfn "Parsing file: %A" filepath
        IO.File.ReadAllText (filepath, Text.Encoding.UTF8)

    let rawInputBlocks(rawData:string) = 
        rawData
        |> (fun input -> 
            input.ReplaceLineEndings("\n").Split([|'\n'|]))

    ParseFile(filePath) |> rawInputBlocks |> List.ofArray

type WINNINGS = Set<int>
type BETS = Set<int>

type Scratchcard = {
    id : int
    copies: int
    winningNumbers : WINNINGS
    betNumbers : BETS
    }

type RawScratchcardData = ((int32 * int32 list) * int32 list)
    
type Scratchcards = Scratchcard list

let ParseScratchcards (rawScratchcards:string list) : Scratchcards = 

    let parseCardId =
        skipString "Card" .>> spaces >>. pint32 .>> skipChar ':' .>> spaces 
        .>>. (many1 (pint32 .>> spaces)) 
        .>>  skipChar '|' .>> spaces
        .>>. (many1 (pint32 .>> spaces))

    let parseRawCardData (rawCardData:string) =
        runParserOnString
            parseCardId
            () "MyParser" rawCardData

    let createScratchcard (result:RawScratchcardData) =
        match result with
        | ((rawId,rawWinningNumbers),rawBetNumbers) -> 
            { 
                id = rawId;
                copies = 0;
                winningNumbers = WINNINGS rawWinningNumbers;
                betNumbers = BETS rawBetNumbers;
            }
        
    List.map parseRawCardData rawScratchcards
    |> List.map (
        fun result -> 
            match result with
            | Success(((rawId,rawWinningNumbers),rawBetNumbers),_,_) ->
                ((rawId,rawWinningNumbers),rawBetNumbers)
            | Failure(errorMsg,_,_) -> failwithf "Failed to parse input: %A" errorMsg
        )
    |> List.map createScratchcard

let findWinningBets (cards:Scratchcards) =

    cards |> 
    List.map 
        (fun (card:Scratchcard) ->
            Set.intersect card.winningNumbers card.betNumbers)

let calculateWinnings (winnings:Set<int> list) : int = 

    let calculateCardWinnings (wins:Set<int>) : int =
        if wins.Count > 0 then int(2.0**((float)(wins.Count-1))) else 0

    List.map calculateCardWinnings winnings
    |> List.sum

    
let public SolvePuzzleA() =
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_04A.txt""")
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_04A.txt""")
    |> ParseScratchcards
    |> findWinningBets
    |> calculateWinnings
    
///////////////////////////////////////////////////// PROBLEM 4B

type ScratchCardWins = {
    cardId : int
    winsCount : int
    }

let assignCopies (scratchCards:Scratchcards) (scratchCardWins:ScratchCardWins list) =
    
    let createCardWins (scratchCards:Scratchcards) (scratchCardWins:ScratchCardWins) =
        let rootScratchcardCopies = 
            scratchCards |> List.find (fun rootCard -> rootCard.id = scratchCardWins.cardId) 
        let scratchCards = 
            scratchCards 
            |> List.map  
                (fun scratchCard -> 
                    if scratchCard.id > scratchCardWins.cardId &&
                       scratchCard.id <= scratchCardWins.cardId + scratchCardWins.winsCount
                       then { 
                            scratchCard with 
                                copies = scratchCard.copies + rootScratchcardCopies.copies + 1 
                            }
                       else scratchCard
                )
        scratchCards

    let scratchcardsAndWins =
        scratchCardWins |> List.fold createCardWins scratchCards
    scratchcardsAndWins

let calculateWinningCards (cards:Scratchcards) : ScratchCardWins list =

    cards |> 
    List.map 
        (fun (card:Scratchcard) ->
            {
                cardId = card.id;
                winsCount = (Set.intersect card.winningNumbers card.betNumbers).Count;
            }
        )

let calculateTotalWins (scratchCards:Scratchcards) =
    let totalCards = scratchCards |> List.sumBy (fun x -> x.copies+1)
    ((*scratchCards, *)totalCards)

let public SolvePuzzleB() =
    let normalizedInput =
        //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_04A_Test.txt""")
        GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_04A.txt""")
    let scratchCards = ParseScratchcards normalizedInput
    scratchCards 
    |> calculateWinningCards
    |> assignCopies scratchCards
    |> calculateTotalWins
