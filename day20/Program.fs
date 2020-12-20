
open day20.Tile
open day20.MapAssembly
open day20.MapBuilder
open day20.MonsterScanner
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
      let edge = edgeSize tiles 
      let tileMap = assemble tiles
      let tile : RawTile = buildMap tileMap edge
      let tileAndMonsters : RawTile*int =
            tile.allOrientations ()
            |> Seq.map (fun tile -> (tile,findMonsters tile))
            |> Seq.filter (fun tileAndMonsters -> snd tileAndMonsters > 0)
            |> Seq.head
      printfn "Tile and monsters: %A" tileAndMonsters
      let monsterTile : RawTile = fst tileAndMonsters
      let monsterCount : int = snd tileAndMonsters
      printfn "Monster size: %d" MONSTER.matchChars
      printfn "Rough sea or monster parts: %A" (monsterTile.countChar '#')
      let roughSea = (monsterTile.countChar '#') - (monsterCount * MONSTER.matchChars)
      printfn "Rough sea: %A" roughSea 
      
let testMap =
      let tile1 = RawTile (1UL, [|"1234";"5ab6";"7cd8";"9123"|])
      let tile2 = RawTile (1UL, [|"2143";"6ef7";"8gh9";"0ij1"|])
      let tile3 = RawTile (1UL, [|"1234";"5kl6";"7mn8";"9123"|])
      let tile4 = RawTile (1UL, [|"2143";"6op7";"8qr9";"0ij1"|])
      let map : TileMap=
            Map.empty
                  .Add((0,0),FastTile(tile1))
                  .Add((1,0),FastTile(tile2))
                  .Add((0,1),FastTile(tile3))
                  .Add((1,1),FastTile(tile4))
      printfn "MAP: %A" (buildMap map 2) 


[<EntryPoint>]
let main argv =
      let tiles = readRaw "/Users/xeno/projects/aoc2020/day20_fs/input.txt"
      // task1 tiles
//      let fast = FastTile(tile)
//      let cropped = cropTile (fast)
//      printfn "%A" cropped 
      task2 tiles  
      0 // return an integer exit code