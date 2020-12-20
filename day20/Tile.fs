module day20.Tile

open System

let reverseString (s:String) : String =
    s.ToCharArray() |> Array.rev |> String.Concat 

type TileId = uint64 // just to not use it by mistake 

type RawTile (id: TileId, lines: String[]) as self =
    override this.ToString() = String.concat "\n" lines |> (sprintf "Tile[%d]: \n%s\n" id)
    member this.Id = id
    member this.Lines = lines 
    member this.left () : int =
        lines
        |> Seq.map (fun line -> (line.ToCharArray ()).[0])
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
        
    member this.leftRev () : int =
        lines
        |> Seq.rev 
        |> Seq.map (fun line -> line.[0])
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
        
        member this.right () : int =
        lines
        |> Seq.map (fun line -> line.[line.Length-1])
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
        
    member this.rightRev () : int =
        lines
        |> Seq.rev 
        |> Seq.map (fun line -> line.[line.Length-1])
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
        
    member this.top () : int =
        lines.[0].ToCharArray ()
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
     
    member this.topRev () : int =
        lines.[0].ToCharArray ()
        |> Seq.rev 
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0

    member this.bottom () : int =
        lines.[lines.Length-1].ToCharArray ()
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
     
    member this.bottomRev () : int =
        lines.[lines.Length-1].ToCharArray ()
        |> Seq.rev 
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
        
    member this.flipHorizontal () : RawTile =
        let newLines = lines |> Array.map (reverseString)
        RawTile (id, newLines)
    
    member this.flipVertical () : RawTile =
        RawTile (id, lines |> Array.rev)
    
    member this.rotateLeft () : RawTile =
        let l = lines.[0].Length-1 
//        printfn "rotateLeft ()" 
        let at(x:int) (y:int) : char =
//            printfn "at(%d,%d) = %A" x y lines.[l-x].[y]
            lines.[l-x].[y]
        let s (y:int): String =
            {0..l}
            |> Seq.map (fun x -> at x y)
            |> String.Concat 
        let newLines = {0..l} |> Seq.map (s) |> Seq.toArray 
        RawTile (id,newLines)
        
    member this.rotateRight () : RawTile = this.rotateLeft().rotateLeft().rotateLeft()
    
    member this.allOrientations () : RawTile[] =
        let frontSide = self 
        let backSide  = self.flipHorizontal () // vertical is faster 
        [|
         frontSide                                 // rotation 1
         frontSide.rotateLeft ()                   // rotation 2
         backSide.flipVertical()                   // rotation 3 (= rotate.rotate)
         frontSide.rotateRight ()                  // rotation 4 
         backSide                                  // rotation 1
         backSide.rotateLeft ()                    // rotation 2
         frontSide.flipVertical()                  // rotation 3 (= rotate.rotate)
         backSide.rotateRight ()                   // rotation 4
        |]
    member this.Height = lines.Length
    member this.Width = lines.[0].Length
    member this.charAt (pos:int*int) = lines.[snd pos].[fst pos]
    member this.countChar (charToMatch:char) =
        let countForLine (s:String) =
            s.ToCharArray()
            |> Seq.filter (fun (c:char) -> c = charToMatch)
            |> Seq.length
        lines |> Seq.map countForLine |> Seq.sum 

type FastTile (id:TileId,top:int,bottom:int,left:int,right:int,raw:RawTile) as self =
    override this.ToString () = sprintf "FastTile(id:%d t:%d,b:%d,l:%d,r:%d)" id top bottom left right
    new(raw:RawTile) = FastTile(raw.Id, raw.top (), raw.bottom (), raw.left (), raw.right (), raw)
    member this.Id = id
    member this.Top = top
    member this.Bottom = bottom
    member this.Left = left
    member this.Right = right
    member this.Raw = raw 
        
