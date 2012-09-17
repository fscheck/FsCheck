module PropertyExamples    
    
    open FsCheck
    open FsCheck.NUnit
    open NUnit.Framework
    open Prop
    
    [<FsCheckProperty>]
    let RevUnit (x:char) = 
        List.rev [x] = [x]

    [<FsCheckProperty>]
    let RevApp (x:string) xs = 
        List.rev (x::xs) = List.rev xs @ [x] 
            |> trivial (xs = [])
            |> trivial (xs.Length = 1)

    [<FsCheckProperty>]
    let MaxLe (x:float) y = 
        (x <= y) ==> (lazy (max  x y = y))

    [<FsCheckProperty( Verbose = true )>]
    let RevIdVerbose (xs:int[]) = 
        Array.rev xs = xs

    [<FsCheckProperty>]
    let Product (x:int, y:int)
        = (x > 0 && y > 0) ==> (x*y > 0)        
    