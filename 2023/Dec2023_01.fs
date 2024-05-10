module Dec2023_01

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

let normalizeInputData(filePath:string) = 

    let rawInputBlocks(filePath:string) = 
        ParseInputFile.ParseFile filePath
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))

    rawInputBlocks(filePath)

let getFirstLastDigitNumber (s:string) : int =
    let allDigits = [|'0'..'9'|]
    let firstDigit = s[s.IndexOfAny(allDigits)] - '0'
    let lastDigit = s[s.LastIndexOfAny(allDigits)] - '0'
    let fullDigit = (int)firstDigit * 10 + (int)lastDigit
    fullDigit

let getCalibrationsTotal (allCalibrations:string[]) : int =
    allCalibrations
    |> Array.fold (fun state calibration -> state  + getFirstLastDigitNumber calibration) 0 
    
let public SolvePuzzleA() =
        
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_01A_Test.txt""")
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_01A.txt""")
    |> getCalibrationsTotal




let getSwitchAlphaToNumerals (s:string) : string =

    let allDigits = [|
        ("1","1");
        ("2","2");
        ("3","3");
        ("4","4");
        ("5","5");
        ("6","6");
        ("7","7");
        ("8","8");
        ("9","9");

        ("1","one");
        ("2","two");
        ("3","three");
        ("4","four");
        ("5","five");
        ("6","six");
        ("7","seven");
        ("8","eight");
        ("9","nine");
        |]

    let alphaDigitReplacement (isForLastLetter:bool) (normalizedString:string) =

        let firstAlphaDigit =
            Array.mapi 
                (fun index (value, alphabeticNumber:string) -> 
                    if not isForLastLetter then (normalizedString.IndexOf(alphabeticNumber), index)
                    else (normalizedString.LastIndexOf(alphabeticNumber), index))
                allDigits
            |> Array.choose (fun (alphaIndex, allDigitsIndex) -> if alphaIndex >= 0 then Some (alphaIndex, allDigitsIndex) else None)
         
        if firstAlphaDigit.Length <= 0 then normalizedString
        else firstAlphaDigit
            |> (fun x -> 
                if not isForLastLetter then 
                    x |> Array.minBy (fun (alphaIndex, allDigitsIndex) -> alphaIndex)
                else
                    x |> Array.maxBy (fun (alphaIndex, allDigitsIndex) -> alphaIndex))
            |> (fun (alphaIndex, allDigitsIndex) -> 
                    let (value, alphabeticNumber) = allDigits[allDigitsIndex]
                    normalizedString.Replace(alphabeticNumber, value))


    alphaDigitReplacement false s
    |> alphaDigitReplacement true

    //Array.fold 
    //    (fun state (value:string, alphabeticNumber:string) -> 
    //        state.Replace(alphabeticNumber, value))
    //    s 
    //    allDigits
    
let normalizeLetterNumbers (s:string) =

    s |> getSwitchAlphaToNumerals


let public SolvePuzzleB() =
    
    //normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_01B_Test.txt""")
    normalizeInputData($"""{__SOURCE_DIRECTORY__}/Dec2023_01A.txt""")
    //[|"fourfivesix1foursgbzzfnggoneighttbk"|]
    |> Array.map normalizeLetterNumbers
    |> getCalibrationsTotal
