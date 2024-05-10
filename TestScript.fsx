[<AutoOpen>]
module MyParsing

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open System.Threading
#r "nuget: fparsec"
open FParsec
open FParsec.CharParsers
open FParsec.Primitives
open FParsec.Error

let GetNormalizedInputData(filePath:string) = 
    let ParseFile (filepath:string) =
        IO.File.ReadAllText (filepath, Text.Encoding.UTF8)

    let rawInputBlocks(filePath:string) = 
        ParseFile filePath
        |> (fun input -> 
            input.ReplaceLineEndings("\n").Split([|'\n'|]))

    rawInputBlocks(filePath)

type GridItemCoords = 
    | NUMBER of pos : int * value : int32
    | SYMBOL 
    | DOT of pos : int

type GridItem = {
    Item : GridItemCoords
    xStart : int
    xEnd : int
    }

let isSymbol (c : char) : bool =
    not(isLetter c) && not(isDigit c) && not(c = '.')

let symbols stream  = satisfy isSymbol

let findAllItemsCoords ((*grid : string array*)) =

    let myParser : Parser<GridItemCoords,unit> = 
        fun stream -> 
            FParsec.Reply(int(stream.IndexOfFirstChar) |> DOT)
    let Dots = 
        printfn "T1 "
        (pchar '.') >>. myParser (*>>% DOT*) .>> spaces
    let NumberItem stream = 
        printfn "T2 "
        let ni = (many1SatisfyL isDigit "NumberItem") .>> spaces
        let num = 0 
        FParsec.Reply((0,num) |> NUMBER)
    let SymbolItem = 
        printfn "T3 "
        satisfy (fun c -> isSymbol (c)) >>% SYMBOL .>> spaces

    //let GridItemsParser:Parser<GridItemCoords list, unit> = many1 (Dots <|> NumberItem <|> SymbolItem)
    let state = ()
    let input = @"467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598.."
    runParserOnString (many1 (Dots <|> NumberItem <|> SymbolItem)) state "MyParser" input

findAllItemsCoords();;

    //let isDigit c = 
    //    match c with
    //    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    //    | _ -> false

    //let findItemAtCoords (grid : string array, y, x) =
    //    let c = grid[y][x]
    //    if isDigit c then 
    //        if x = 0 || not (isDigit (grid[y][x-1])) then 
    //            NUMBER 0
    //        else DOT
    //    else 
    //        match c with
    //        | '.' -> DOT
    //        | any -> SYMBOL

    //for y in [0..grid.Length-1] do   
    //    for x in [0..grid[0].Length-1] do
    //        match findItemAtCoords (grid, y, x) with
    //        | NUMBER n -> ()
    //        | _ -> 0
    //    printfn ""

let public SolvePuzzleA() =
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_03A_Test.txt""")
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_02A.txt""")
    //|> findAllItemsCoords


let public SolvePuzzleB() =
    GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_03A_Test.txt""")
    //GetNormalizedInputData($"""{__SOURCE_DIRECTORY__}/2023/Dec2023_02A.txt""")

let x = runParserOnString (pstring ".") () "myParser" ".ALLO";;
match x with
| Success (res, state, pos) ->
    printfn "state: %A" state
    printfn "pos: %A" pos
| Failure (msg, err, state) ->
    printfn "msg: %A" msg
    printfn "err: %A" err
    printfn "state: %A" state

SolvePuzzleA()
//SolvePuzzleB()
