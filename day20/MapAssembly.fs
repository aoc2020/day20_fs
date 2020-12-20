module day20.MapAssembly

open day20.Tile

type Used = Set<TileId>
type Tiles = FastTile[]
type Pos = int*int
type TileMap = Map<Pos,FastTile>

let edgeSize (tiles:RawTile[]) =
    let rec findSquare (n:int) =
        if n * n = tiles.Length then n
        else findSquare (n+1)
    findSquare 2

let expandToFast (tiles:RawTile[]) : FastTile[] =
    tiles
    |> Array.map (fun (t:RawTile) -> t.allOrientations ())
    |> Array.concat 
    |> Array.map (FastTile)

let initialPositionsToFill (edge:int) : List<Pos> = 
    let r = [|0..edge-1|]
    let positions = Seq.allPairs r r |> Seq.toList
    printfn "Positions: %A" positions
    positions 

let canBeAt (pos:Pos) (map:TileMap) (tile:FastTile) =
    let checkTop () = map.[(fst pos,(snd pos)-1)].Bottom = tile.Top
    let checkLeft () = map.[((fst pos)-1,snd pos)].Right = tile.Left        
    match pos with
    | (0,0) -> true
    | (0,_) -> checkTop ()
    | (_,0) -> checkLeft ()
    | (_,_) -> checkTop () && checkLeft () 

let rec solve (tiles:FastTile[]) (positions:List<Pos>)  (used:Used) (map:TileMap) : seq<TileMap> =
//    printfn "Solve: positions: %A map: %A" positions map 
    let isNotUsed (tile:FastTile) = used.Contains tile.Id |> not 
    let tryWithTile (pos:Pos) (tile:FastTile) : seq<TileMap> =
        let newMap = map.Add(pos,tile)
        let newUsed = used.Add tile.Id
        solve tiles positions.Tail newUsed newMap     
    match positions with
    | [] -> [|map|] |> Seq.ofArray   // is already solved
    | pos::tail ->
        tiles
        |> Seq.filter (canBeAt pos map)
        |> Seq.filter isNotUsed 
        |> Seq.map (tryWithTile pos)
        |> Seq.concat 
  
let assemble (raws:RawTile[]) : TileMap =
    let edge = edgeSize raws 
    let tiles = expandToFast raws
    // tiles |> Array.map (printfn "Tile: %A")
    let positions =  initialPositionsToFill (edge)
    printfn "Edge: %d" edge    
    let solution = solve tiles positions Set.empty Map.empty |> Seq.head 
//    printfn "Solution.size= %A" solution.Length
    solution 
    
