
open day20.Tile
open day20.MapAssembly
open day20.IO

let task1 (tiles:RawTile[]) =
      let tileMap = assemble tiles
      let l = tileMap
      printfn "TileMap: %A" tileMap 
      

[<EntryPoint>]
let main argv =
      let tiles = readRaw "/Users/xeno/projects/aoc2020/day20_fs/input.txt"
      task1 tiles 
//      printfn "Original tiles %A" tiles 
//      let tileMap = assemble tiles
      
//      let tile = RawTile(42UL,[|"##.";"#.#";".##"|])
//      printfn "Tile: %A" tile
//      printfn "Fast: %A" (FastTile(tile))
//    let tile = RawTile(2UL,[|"abc";"def";"geh"|])
//    printfn "RotateLeft %A" (tile.rotateLeft ()) 
//    printfn "flipHor:   %A" (tile.flipHorizontal ())
//    printfn "flipVert:  %A" (tile.flipVertical ())
      0 // return an integer exit code