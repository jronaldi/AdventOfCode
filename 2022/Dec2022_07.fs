module Dec2022_07

open System
open System.Text.RegularExpressions

type TerminalStream = 
    | Cd of dir:string
    | Ls
    | Dir of dir:string
    | Content of size:int * name:string

let normalizeInputData() = 

    let parseTerminalStream (line:string list) =
        let parseTerminalCommand (command:string list) =
            match command.Head.ToLowerInvariant() with
            | "cd" -> Cd (dir=command[1])
            | "ls" -> Ls
            | _ -> failwith "Unknown terminal command"

        match line.Head.ToLowerInvariant() with
        | "$" -> parseTerminalCommand line.Tail
        | "dir" -> Dir (dir=line.Tail.Head)
        | fileSize when Regex.IsMatch(fileSize, "\d+") -> 
            Content (size = int fileSize, name=line.Tail.Head)
        | _ -> failwith "Unknown terminal command output"

    let rawInputBlocks = 
        ParseInputFile.ParseFile $"""{__SOURCE_DIRECTORY__}/Dec2022_07_Test.txt"""
        |> (fun input -> input.ReplaceLineEndings("\n").Split([|'\n'|]))
        |> Array.map (fun input -> input.Split([|' '|]) |> List.ofArray)
        |> List.ofArray
        |> (fun x -> x)
        |> List.map parseTerminalStream

    rawInputBlocks

type DirTree =
    | Folder of name:string * size:int * children:DirTree list
    | File of name:string * size:int

let BuildDirectoryTree (terminalStream:TerminalStream list) =

    let rec treeAppend (tree:DirTree) (dir:string) (item:DirTree) =
        match tree with
        | Folder (name, size, children) -> 
            if name = dir then Folder (name, size, item :: children)
            else Folder (name, size, children)
        | File (name, size) -> File (name, size)

    let rec buildDirectoryTree (tree:DirTree, cd:string list) (terminalStream:TerminalStream) =
        
        match terminalStream with
        | Cd folder -> 
            match folder with
            | ".." ->
                let cd = cd.Tail
                (tree, cd)
            | _ ->
                let cd = folder :: cd 
                (tree, cd)
        | Ls -> 
            (tree, cd)
        | Dir dir -> 
            let tree = treeAppend tree cd.Head (Folder (dir, -1, []))
            (tree, cd)
        | Content (size, name) -> 
            let tree = treeAppend tree cd.Head (File (name, size))
            (tree, cd) 

    List.fold buildDirectoryTree (Folder ("", -1, []), []) terminalStream    


let public SolvePuzzleA() =
               
    normalizeInputData()
    |> BuildDirectoryTree

let public SolvePuzzleB() = 

    normalizeInputData()
