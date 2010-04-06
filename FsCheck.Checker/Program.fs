
open FsCheck
open System

printfn "%A" Runner.init.Value
printfn "%A" (Gen.register<Checks.Helpers.Arbitraries>())

Check.QuickAll <| Type.GetType( "FsCheck.Checks.Random, FsCheck", true)

Check.QuickAll <| Type.GetType( "FsCheck.Checks.Common, FsCheck", true)

Check.QuickAll <| Type.GetType( "FsCheck.Checks.Generator, FsCheck", true)

Check.QuickAll <| Type.GetType( "FsCheck.Checks.Functions, FsCheck", true)

Check.QuickAll <| Type.GetType( "FsCheck.Checks.Arbitrary, FsCheck", true)

Check.QuickAll <| Type.GetType( "FsCheck.Checks.Property, FsCheck", true)

printfn "Done. Press any key to exit."
Console.ReadKey() |> ignore
