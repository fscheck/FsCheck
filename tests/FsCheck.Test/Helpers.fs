namespace FsCheck.Test

[<assembly: Xunit.CollectionBehavior(Xunit.CollectionBehavior.CollectionPerAssembly)>]
do()

module Helpers = 

    open FsCheck.FSharp

    
    
    let isIn l elem = List.exists ((=) elem) l

    let assertTrue pred = if not pred then failwith "assertion failure"
