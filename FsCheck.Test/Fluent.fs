namespace FsCheck.Test

module Fluent_Tests =

    open System
    open Xunit
    open FsCheck

    open FsCheck.Xunit
    open Helpers
        
    [<Fact>]
    let ``Any_OfType should be able to create an generator without previous runs``() =
        // remarks: the problem is that Runner.init is never called
        // I think we should do this in Fluent.Any.OfType should do
        let filter z = z % 3 = 0
        let filterFunc = new Func<_,_>(filter)
        let gen = Fluent.Any.OfType<int>()
        true
        
