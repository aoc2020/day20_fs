module day20.IO

open System
open System.IO
open day20.Tile

let readFile (file:String) = seq {
    use sr = new StreamReader(file)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let rec splitByBlank(lines: String[]) : List<List<String>> =
    let init = [[]]
    let accumulate (acc:list<list<String>>) (value:String) =
                    if value = "" then
                        [] :: acc  
                    else
                        match acc with
                        | head::tail -> (value::head)::tail
    let res = lines |> Seq.fold accumulate init 
    res |> List.map (List.rev) |> List.rev // all is backwards, so deep reverse     
            

let toRawTile (s: List<String>) : RawTile =
    let lines = s.Tail |> Seq.toArray
    let id = ((s.Head.Split " ").[1].Split ":").[0] |> int
    let tile = RawTile (id, lines)
    tile 

let readRaw (file:String) : RawTile[] =
    let lines = readFile(file) |> Seq.toArray
    let split = splitByBlank lines
    let rawTiles = split |> Seq.map toRawTile |> Seq.toArray  
//    printfn "%A" rawTiles
    rawTiles 