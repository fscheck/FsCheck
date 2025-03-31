namespace FsCheck.Test

[<assembly: Xunit.CollectionBehavior(Xunit.CollectionBehavior.CollectionPerAssembly)>]
do()

module Helpers = 

    open FsCheck.FSharp

    let sample n = Gen.sampleWithSize 1000 n

    let sample1 gn = sample 1 gn |> Seq.head
    
    let isIn l elem = List.exists ((=) elem) l

    let assertTrue pred = if not pred then failwith "assertion failure"
