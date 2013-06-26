namespace FsCheck.Test

module Fluent =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Fluent
    open FsCheck.Xunit
    open Helpers
        
    [<Fact>]
    let ``Any_OfType should create a generator without previous runs``() =
        //bug fix - initialization issue
        Any.OfType<int>() |> ignore
        
        
