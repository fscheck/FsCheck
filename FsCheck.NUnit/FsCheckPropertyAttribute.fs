namespace FsCheck.NUnit

open System
open NUnit.Framework
open FsCheck

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type PropertyAttribute() =
    inherit TestAttribute()

    let mutable maxTest = Config.Default.MaxTest
    let mutable maxFail = Config.Default.MaxFail    
    let mutable startSize = Config.Default.StartSize
    let mutable endSize = Config.Default.EndSize
    let mutable verbose = false
    let mutable arbitrary = Config.Default.Arbitrary |> List.toArray

    member x.MaxTest with get() = maxTest and set(v) = maxTest <- v
    member x.MaxFail with get() = maxFail and set(v) = maxFail <- v
    member x.StartSize with get() = startSize and set(v) = startSize <- v
    member x.EndSize with get() = endSize and set(v) = endSize <- v
    member x.Verbose with get() = verbose and set(v) = verbose <- v
    member x.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v

