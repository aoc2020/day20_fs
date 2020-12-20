module day20.MonsterScanner

open System
open day20.Tile

let MONSTER_PATTERN : String[] = [|"                  # "
                                   "#    ##    ##    ###"
                                   " #  #  #  #  #  #   "|]
type Pos = int*int
let addPositions (pos1:Pos) (pos2:Pos) = (fst pos1+fst pos2, snd pos1+snd pos2)


type PatternChar (relativePos:Pos,char:Char) as self =
    member this.ToString = sprintf "{+%A=%c}" relativePos char
    member this.matches (tile:RawTile) (basePos:Pos) =
        let pos : Pos = addPositions basePos relativePos
        let target : char = tile.charAt pos 
        if char = '#' then target = '#' else true
    member this.isRequired = char = '#'

let toPatternChars (lines:String[]) =
    let toPatternChar (y:int) (x:int) (c:char) =
        let pos = (x,y)
        PatternChar (pos,c)
    lines    
    |> Seq.mapi (fun y line -> line |> Seq.mapi (toPatternChar y))
    |> Seq.concat
    |> Seq.toArray 

type Pattern(pattern:String[]) as self =
    override this.ToString() = "Pattern"
    member this.Length = pattern.[0].Length
    member this.Height = pattern.Length 
    member this.getPatternChars () = toPatternChars(pattern)
    member this.getMatchers(tile:RawTile) : (Pos -> bool)[] =
        this.getPatternChars()
        |> Seq.filter (fun pc -> pc.isRequired)
        |> Seq.map (fun pc -> pc.matches tile)
        |> Seq.toArray
    member this.matchChars : int =
        let stringChars (s:String) =
            s.ToCharArray ()
            |> Seq.filter (fun (c:char) -> c = '#')
            |> Seq.length         
        pattern |> Seq.map stringChars |> Seq.sum  
        
        
let MONSTER = Pattern(MONSTER_PATTERN) 

let possiblePositions (tile: RawTile) (monster:Pattern) : seq<Pos> =
    let maxX = tile.Width - monster.Length  // -1
    let maxY = tile.Height - monster.Height  // -1
    Seq.allPairs {0..maxX} {0..maxY} 

let all<'T> (f:'T -> bool) (stream: seq<'T>) : bool =
    let not_f t = f t |> not
    stream |> Seq.exists not_f |> not    

let findMonsters (tile: RawTile) : int =
    let poss = possiblePositions tile MONSTER
    let matchers : (Pos -> bool)[] = MONSTER.getMatchers tile
    let isMonster (pos:Pos) : bool = matchers |> all (fun matches -> matches pos) 
    poss |> Seq.filter isMonster |> Seq.length