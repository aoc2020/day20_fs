
open day20.Tile
open day20.MapAssembly
open day20.IO

let task1 (tiles:RawTile[]) =
      let edge = edgeSize tiles 
      let tileMap = assemble tiles
      let corner1 = tileMap.[(0,0)]
      let corner2 = tileMap.[(0,edge-1)]
      let corner3 = tileMap.[(edge-1,0)]
      let corner4 = tileMap.[(edge-1,edge-1)]
      let answer = corner1.Id * corner2.Id * corner3.Id * corner4.Id  
      printfn "TileMap: %A" tileMap
      printfn "Answer (1) : %d" answer
     
let task2 (tiles:RawTile[]) =
      let tileMap = assemble tiles
      1 

[<EntryPoint>]
let main argv =
      let tiles = readRaw "/Users/xeno/projects/aoc2020/day20_fs/input.txt"
      task1 tiles 
      0 // return an integer exit code