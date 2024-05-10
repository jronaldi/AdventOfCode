module ParseInputFile

open System
open System.Threading

let ParseFile (filepath:string) =
        IO.File.ReadAllText (filepath, Text.Encoding.UTF8)
