module Dec2023_03 // Problem link: https://adventofcode.com/2023/day/3

open System
open FParsec

let GetNormalizedInputData(filePath:string) = 
    let ParseFile (filepath:string) =
        printfn "Parsing file: %A" filepath
        IO.File.ReadAllText (filepath, Text.Encoding.UTF8)

    //let rawInputBlocks(filePath:string) = 
    //    ParseFile filePath
    //    |> (fun input -> 
    //        input.ReplaceLineEndings("\n").Split([|'\n'|]))

    ParseFile(filePath)

type Location = { x : int ; y : int }
type PartNumberInfo = { partLocations : Location list; partNumber : int32 }

type GridItemCoords = 
    | NUMBER of colFirst : int * colLast : int * line : int * value : int32
    | SYMBOL of col : int * line : int * symb : char
    | DOT of col : int * line : int

let isSymbol (c : char) : bool =
    not(isLetter c) && not(isDigit c) && not(c = '.')

let findAllItemsCoords (sourceData : string) =

    let myDotParser : Parser<GridItemCoords,unit> = 
        fun stream -> 
            FParsec.Reply((int(stream.Column)-2,int(stream.Line)-1) |> DOT)

    let Dots = (pchar '.') >>. myDotParser .>> spaces

    let mySymbolParser : Parser<GridItemCoords,unit> = 
        fun stream -> FParsec.Reply((int(stream.Column)-2,int(stream.Line)-1, stream.Peek(-1)) |> SYMBOL)

    let SymbolItem = satisfy (fun c -> isSymbol (c)) >>. mySymbolParser .>> spaces

    let myPartNumberParser (stream:CharStream<unit>) : Reply<GridItemCoords> =
            let mutable offset = -1
            let pos = stream.Index
            while (int(stream.Index) + offset >=0 && isDigit(stream.Peek(offset))) do 
                offset <- offset-1;
            let numberLength = -offset

            let partNumber = 
                new string([|for x in [offset+1..-1] -> stream.Peek(x)|])
                |> int32
            
            FParsec.Reply((int(stream.Column)-numberLength,int(stream.Column)-2,int(stream.Line)-1,partNumber) |> NUMBER)

    let NumberItem : Parser<GridItemCoords,unit> = 
        (many1SatisfyL isDigit "NumberItem") >>. myPartNumberParser

    let rawParsedData = runParserOnString (many (Dots <|> NumberItem <|> SymbolItem)) () "MyParser" sourceData
    match rawParsedData with
    | Success(result,_,_) -> result
    | Failure(errorMsg,_,_) -> failwithf "Parsing failed: %A" errorMsg

let CalculateSumOfValidPartNumbers (rawSchematicData:GridItemCoords list) =
    
    let extractSymbolLocations (rawSchematicData:GridItemCoords list) =
        rawSchematicData
        |> List.choose 
            (fun x -> 
                match x with
                | SYMBOL (col, line, _) -> Some {x=col; y=line}
                | _ -> None
                )
        |> Set

    let extractPartsInfo (rawSchematicData:GridItemCoords list) : PartNumberInfo list =
        rawSchematicData
        |> List.choose 
            (fun x -> 
                match x with
                | NUMBER (colFirst,colLast,line,value) -> 
                    let parsedPartNumber = 
                        { 
                            partLocations = [for x in [colFirst..colLast] -> {Location.x=x;Location.y=line}];
                            partNumber = value;
                        }
                    Some parsedPartNumber
                | _ -> None
                )
    
    let symbolLocations = extractSymbolLocations rawSchematicData
    let partsInfo = extractPartsInfo rawSchematicData

    let symbolOffsets = [(-1,-1); (-0,-1); (+1,-1);
                         (-1, 0);          (+1, 0);
                         (-1,+1); (-0,+1); (+1,+1)]

    let testValidSymbolLocation (partLocation:Location) (symbolOffset:int * int) =
        let xOffset,yOffset = symbolOffset
        symbolLocations.Contains {x=partLocation.x+xOffset; y=partLocation.y+yOffset}

    let validateExistsSymbolNearby (partLocation:Location) =
        List.exists (testValidSymbolLocation partLocation) symbolOffsets
    
    let validatePartNumber (partNumberInfo:PartNumberInfo) =
        if List.exists validateExistsSymbolNearby partNumberInfo.partLocations then Some partNumberInfo.partNumber
        else None

    let validateAllPartNumbers (partNumbersInfo:PartNumberInfo list) =
        List.choose validatePartNumber partNumbersInfo
    
    let validPartNumbers = validateAllPartNumbers partsInfo
   
    // Tracing all outputs:
    // (symbolLocations, partsInfo, rawSchematicData,validPartNumbers,List.sum validPartNumbers)
    List.sum validPartNumbers

let public SolvePuzzleA() =
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_03A_Test.txt""")
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_03A.txt""")
    |> findAllItemsCoords
    |> CalculateSumOfValidPartNumbers

let starsWithTwoPartNumbers (rawSchematicData:GridItemCoords list) = 

    let extractStarSymbolLocations (rawSchematicData:GridItemCoords list) =
        rawSchematicData
        |> List.choose 
            (fun x -> 
                match x with
                | SYMBOL (col, line, symb) when symb = '*' -> Some {x=col; y=line}
                | _ -> None
                )
    
    let extractPartsInfo (rawSchematicData:GridItemCoords list) : PartNumberInfo list =
        rawSchematicData
        |> List.choose 
            (fun x -> 
                match x with
                | NUMBER (colFirst,colLast,line,value) -> 
                    let parsedPartNumber = 
                        { 
                            partLocations = [for x in [colFirst..colLast] -> {Location.x=x;Location.y=line}];
                            partNumber = value;
                        }
                    Some parsedPartNumber
                | _ -> None
                )

    let symbolOffsets = [(-1,-1); (-0,-1); (+1,-1);
                         (-1, 0);          (+1, 0);
                         (-1,+1); (-0,+1); (+1,+1)]

    // For all stars
    //  Find all adjacent part numbers
    //      

    let validatePartNumberAtOffset (partInfo:PartNumberInfo) (testedPartNumberLoc:Location) : int32 option =
        List.exists (fun partLoc -> partLoc = testedPartNumberLoc) partInfo.partLocations
            |> (fun exists -> 
                if exists then Some partInfo.partNumber 
                else None)

    let testAllAdjacentSymbolPositions (starLocation:Location) (partInfo:PartNumberInfo)  = 
        symbolOffsets 
        |> List.map
            (fun (xOffset, yOffset) -> {x=starLocation.x + xOffset; y=starLocation.y + yOffset})
        |> List.choose (validatePartNumberAtOffset partInfo)

    let validateExistsTwoAdjacentPartNumbers (partInfos:PartNumberInfo list) (starLocation:Location) =
         
        let calculateGearRatio (gears:int32 list) = gears.Head * gears.Tail.Head

        List.collect (testAllAdjacentSymbolPositions starLocation) partInfos
        |> List.distinct
        |> (fun matches -> if matches.Length = 2 then Some (calculateGearRatio matches) else None)

    let partInfos = extractPartsInfo rawSchematicData
    extractStarSymbolLocations rawSchematicData
    |> List.choose (validateExistsTwoAdjacentPartNumbers partInfos)
    |> List.sum

let public SolvePuzzleB() =
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_03A_Test.txt""")
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_03A.txt""")
    |> findAllItemsCoords
    |> starsWithTwoPartNumbers
