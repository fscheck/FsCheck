#light

open FsCheck
open System

quickCheckAll <| Type.GetType( "FsCheck.Checks.Random, FsCheck", true)

Console.ReadKey() |> ignore

quickCheckAll <| Type.GetType( "FsCheck.Checks.Arbitrary, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Common, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Generator, FsCheck", true)

quickCheckAll <| Type.GetType( "FsCheck.Checks.Functions, FsCheck", true)

Console.ReadKey() |> ignore
