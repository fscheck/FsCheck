#light

open FsCheck
open System

do Gen.register<Checks.Helpers.Arbitraries>()

quickCheckAll <| Type.GetType( "FsCheck.Checks.Random, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Common, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Generator, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Functions, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Arbitrary, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Property, FsCheck", true)

printfn "Done. Press any key to exit.";Console.ReadKey() |> ignore
