module day20.Tile

open System

let reverseString (s:String) : String =
    s.ToCharArray() |> Array.rev |> String.Concat 

type RawTile (id: int, lines: String[]) as self =
    override this.ToString() = String.concat "\n" lines |> (sprintf "Tile[%d]: \n%s\n" id)
    member this.left () : int =
        lines
        |> Seq.map (fun line -> line.[0])
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
        lines.[0].ToCharArray ()
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
     
    member this.bottomRev () : int =
        lines.[0].ToCharArray ()
        |> Seq.rev 
        |> Seq.map (fun c -> if c = '#' then 1 else 0) 
        |> Seq.fold (fun (acc:int) (i:int) -> (acc*2)+i) 0
        
    member this.flipHorizontal () : RawTile =
        let newLines = lines |> Array.map (reverseString)
        RawTile (id, newLines)
    
    member this.flipVertical () : RawTile =
        RawTile (id, lines |> Array.rev)
    
    member this.rotateLeft () : RawTile =
        let s (n:int): String =
            {9..0}
            |> Seq.map (fun (i:int) -> lines.[i].[0])
            |> String.Concat
        let newLines = {0..9} |> Seq.map (s) |> Seq.toArray 
        RawTile (id,newLines)
        
    member this.rotateRight () : RawTile = this.rotateLeft().rotateLeft().rotateLeft()
        
        
        
