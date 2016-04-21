// Learn more about F# at http://fsharp.org

open System

open FsCheck

[<EntryPoint>]
let main argv = 
    printfn "Hello World!"
    printfn "%A" argv
    
    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
    
    Check.Quick revRevIsOrig
    
    0 // return an integer exit code
