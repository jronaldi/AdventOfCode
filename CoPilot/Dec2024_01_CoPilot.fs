module Dec2024_01CoPilot

open System
open System.IO

// Function to read the input from a text file and return a list of pairs of integers
let readInputFromFile (filePath: string) =
    File.ReadAllLines(filePath)
    |> Array.map (fun line -> line.Split(' ',StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> List.ofArray)
    |> List.ofArray

// Read the input from a text file
//let input = readInputFromFile $"""{__SOURCE_DIRECTORY__}/Dec2024_01A_Test.txt"""
let input = readInputFromFile $"""{__SOURCE_DIRECTORY__}/Dec2024_01A.txt"""

// Extract the left and right numbers into separate lists
let leftList = input |> List.map (fun pair -> pair.[0])
let rightList = input |> List.map (fun pair -> pair.[1])

// Sort the lists
let sortedLeftList = List.sort leftList
let sortedRightList = List.sort rightList

// Calculate distances
let distances = List.map2 (fun left right -> abs (left - right)) sortedLeftList sortedRightList
let totalDistance = List.sum distances

let SolvePuzzleA() =
    // Print the distances and the total distance
    printfn "List of distances: %A" distances
    printfn "Total distance: %d" totalDistance
