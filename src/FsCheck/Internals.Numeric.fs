namespace FsCheck.Internals

/// Adapted slightly from FSharpPlus, https://github.com/fsprojects/FSharpPlus/blob/master/LICENSE.md

module Numeric =

    open System

    type MinValue =
        static member MinValue (_:unit          , _:MinValue) = ()
        static member MinValue (_:bool          , _:MinValue) = false
        static member MinValue (_:char          , _:MinValue) = Char.MinValue
        static member MinValue (_:byte          , _:MinValue) = Byte.MinValue
        static member MinValue (_:sbyte         , _:MinValue) = SByte.MinValue
        static member MinValue (_:float         , _:MinValue) = Double.MinValue
        static member MinValue (_:int16         , _:MinValue) = Int16.MinValue
        static member MinValue (_:int           , _:MinValue) = Int32.MinValue
        static member MinValue (_:int64         , _:MinValue) = Int64.MinValue
        static member MinValue (_:float32       , _:MinValue) = Single.MinValue
        static member MinValue (_:uint16        , _:MinValue) = UInt16.MinValue
        static member MinValue (_:uint32        , _:MinValue) = UInt32.MinValue
        static member MinValue (_:uint64        , _:MinValue) = UInt64.MinValue
        static member MinValue (_:decimal       , _:MinValue) = Decimal.MinValue
        static member MinValue (_:DateTime      , _:MinValue) = DateTime.MinValue
        static member MinValue (_:DateTimeOffset, _:MinValue) = DateTimeOffset.MinValue
        static member MinValue (_:TimeSpan      , _:MinValue) = TimeSpan.MinValue

        static member inline Get() =
            let inline call2 (a:^a, b:^b) = ((^a or ^b) : (static member MinValue: _*_ -> _) b, a)
            let inline call (a:'a) = call2 (a, Unchecked.defaultof<'r>) :'r
            call Unchecked.defaultof<MinValue>


    type MaxValue =
        static member MaxValue (_:unit          , _:MaxValue) = ()
        static member MaxValue (_:bool          , _:MaxValue) = true
        static member MaxValue (_:char          , _:MaxValue) = Char.MaxValue
        static member MaxValue (_:byte          , _:MaxValue) = Byte.MaxValue
        static member MaxValue (_:sbyte         , _:MaxValue) = SByte.MaxValue
        static member MaxValue (_:float         , _:MaxValue) = Double.MaxValue
        static member MaxValue (_:int16         , _:MaxValue) = Int16.MaxValue
        static member MaxValue (_:int           , _:MaxValue) = Int32.MaxValue
        static member MaxValue (_:int64         , _:MaxValue) = Int64.MaxValue
        static member MaxValue (_:float32       , _:MaxValue) = Single.MaxValue
        static member MaxValue (_:uint16        , _:MaxValue) = UInt16.MaxValue
        static member MaxValue (_:uint32        , _:MaxValue) = UInt32.MaxValue
        static member MaxValue (_:uint64        , _:MaxValue) = UInt64.MaxValue
        static member MaxValue (_:decimal       , _:MaxValue) = Decimal.MaxValue
        static member MaxValue (_:DateTime      , _:MaxValue) = DateTime.MaxValue
        static member MaxValue (_:DateTimeOffset, _:MaxValue) = DateTimeOffset.MaxValue
        static member MaxValue (_:TimeSpan      , _:MaxValue) = TimeSpan.MaxValue

        static member inline Get() =
            let inline call2 (a:^a, b:^b) = ((^a or ^b) : (static member MaxValue: _*_ -> _) b, a)
            let inline call (a:'a) = call2 (a, Unchecked.defaultof<'r>) :'r
            call Unchecked.defaultof<MaxValue>

