module day20.MapBuilder

open System
open day20.MapAssembly
open day20.Tile

let cropTile (tile:FastTile): RawTile =
    let trimLine (s:String) = s.[1..s.Length-2] |> String.Concat 
    let lines = tile.Raw.Lines
    let trim1 = lines.[1..lines.Length-2]
    let trim2 = trim1 |> Array.map trimLine   
    RawTile(tile.Id,trim2)

let lineAt (y:int) (tile:RawTile) = tile.Lines.[y]
    
    
let buildRow (map:TileMap) (edge:int) (row:int) : RawTile =
    let xs = [|0..edge-1|]
    let tiles = xs
                |> Seq.map (fun x -> map.[(x,row)])
                |> Seq.map cropTile
                |> Seq.toArray
    let height = tiles.[0].Height
    let ys = [|0..height-1|]
    let lines = ys |> Array.map (fun (y:int) -> tiles |> Array.map (lineAt y) |> String.Concat)
    printfn "Lines: %A" lines
    RawTile(row |> uint64, lines) 

let mergeRows (tiles:RawTile[]) : RawTile =
    let lines (tile:RawTile) = tile.Lines |> Array.toSeq 
    let lines : String[] =
        tiles
        |> Seq.map lines 
        |> Seq.reduce (Seq.append)
        |> Seq.toArray 
    RawTile(0UL,lines)
    

let buildMap (map: TileMap) (edge:int): RawTile =
    let rows = [|0..edge-1|]
    let rowTiles = rows |> Array.map (buildRow map edge)
    let fullMap = mergeRows rowTiles
    printfn "Full map: %A" fullMap  
    fullMap 
    

