
namespace Xunit

open System

type Assert = class end

//    ///Verifies that the exact exception is thrown (and not a derived exception type). This overload is for usage from F#.
//    static member ThrowsFs( testCode:unit -> 'Return ) = 
//        Xunit.Assert.Throws(fun () -> testCode |> box)
//
//    ///Verifies that the exact exception is thrown (and not a derived exception type). This overload is for usage from F#.
//    static member ThrowsFs<'Return>( exceptionType:Type, testCode:unit -> 'Return ) = 
//        Xunit.Assert.Throws(exceptionType, fun () -> testCode |> box)

