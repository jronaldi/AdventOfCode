module NormalizeInput

open System

let NormalizeInputLines (input:string) =
  input.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
