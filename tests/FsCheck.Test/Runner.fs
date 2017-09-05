namespace FsCheck.Test

module RunnerInternals =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Runner
    open System.Linq
    open FsCheck.Xunit

    type ProbeRunner () =
        let mutable result = None
        let sink :Collections.Generic.List<obj> = ResizeArray ()
        let cmpTestData = function 
            { NumberOfTests = nt1; NumberOfShrinks = ns1; Stamps = sx1; Labels = lx1 }, 
            { NumberOfTests = nt2; NumberOfShrinks = ns2; Stamps = sx2; Labels = lx2 } -> 
                nt1 = nt2 && ns1 = ns2 && Enumerable.SequenceEqual (sx1, sx2) && Enumerable.SequenceEqual (lx1, lx2)
        let cmpOutcome = function
            | Outcome.Timeout _, Outcome.Timeout _ -> true
            | Outcome.Exception ex1, Outcome.Exception ex2 -> ex1.Equals ex2
            | Outcome.False, Outcome.False -> true
            | Outcome.True, Outcome.True -> true
            | Outcome.Rejected, Outcome.Rejected -> true
            | _, _ -> false
        member __.Result = result.Value
        member __.Sink = sink
        override __.Equals other =
            match other with
            | :? ProbeRunner as pr -> 
                let resultEq = 
                    match pr.Result, __.Result with
                    | TestResult.True (td1, so1), TestResult.True (td2, so2) -> 
                        so1 = so2 && cmpTestData (td1, td2)
                    | TestResult.False (td1, oa1, sa1, o1, r1), TestResult.False (td2, oa2, sa2, o2, r2) -> 
                        cmpTestData (td1, td2) && 
                        cmpOutcome (o1, o2) && 
                        Enumerable.SequenceEqual (oa1, oa2) && 
                        Enumerable.SequenceEqual (sa1, sa2) &&
                        r1 = r2
                    | TestResult.Exhausted td1, TestResult.Exhausted td2 -> cmpTestData (td1, td2)
                    | _, _-> false
                resultEq && Enumerable.SequenceEqual (pr.Sink, sink)
            | _ -> false
        interface IRunner with
            override __.OnStartFixture _ = ()
            override __.OnArguments (ntest, args, _) = 
                sink.Add ntest
                sink.AddRange args
            override __.OnShrink(args, _) = 
                sink.Add -1
                sink.AddRange args
            override __.OnFinished(_, testResult) = 
                result <- Some testResult


    let check arb body config =
        let p = Prop.forAll arb body
        let seed = Random.create () |> Some
        printfn "seed %A" seed
        let runner1 = ProbeRunner () 
        FsCheck.Runner.check { config with Replay = seed; Runner = runner1; ParallelRunConfig = Some { MaxDegreeOfParallelism = Environment.ProcessorCount } } p
        let runner2 = ProbeRunner () 
        FsCheck.Runner.check { config with Replay = seed; Runner = runner2; ParallelRunConfig = None } p
        runner1.Equals runner2
            
    [<Fact>]
    let ``parallelTest produces same sequence as test on success`` () =
        let body i = true
        let arb = Arb.from<int>
        let config = { Config.Quick with MaxTest = 30000; EndSize = 30000 }
        if not <| check arb body config then failwith "assertion failure!"

    [<Fact>]
    let ``parallelTest produces same sequence as test on failure`` () =
        let body i = i < 999
        let arb = Arb.from<int>
        let config = { Config.Quick with MaxTest = 30000; EndSize = 30000 }
        if not <| check arb body config then failwith "assertion failure!"
            
    [<Fact>]
    let ``parallelTest produces same sequence as test on discard`` () =
        let body _ = true
        let arb = Arb.fromGen (Gen.constant 1 |> Gen.map (fun _ -> Prop.discard ()))
        let config = { Config.Quick with MaxTest = 30000; MaxFail = 100000; EndSize = 30000 }
        if not <| check arb body config then failwith "assertion failure!"
    
    type Tree = Leaf of int | Branch of Tree * Tree
    
    let tree =
        let rec tree' s = 
            match s with
            | 0 -> Gen.map Leaf Arb.generate<int>
            | n when n>0 -> 
                let subtree = tree' (n/2)
                Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                            Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
            | _ -> invalidArg "s" "Only positive arguments are allowed"
        Gen.sized tree'
        
    type TreeGen =
        static member Tree() =
            {new Arbitrary<Tree>() with
                override x.Generator = tree
                override x.Shrinker t = Seq.empty }

    [<Fact>]
    let ``parallelTest works with custom arb`` () =
        Arb.register<TreeGen> ()
        let arb = Arb.from<list<Tree>>
        let body (xs:list<Tree>) = List.rev(List.rev xs) = xs
        let config = { Config.Quick with MaxTest = 3000; EndSize = 3000 }
        if not <| check arb body config then failwith "not threadsafe"
    
    type Integer = Integer of int
    
    let integer =
        let mutable i = 0
        let rec integer' s = 
            i <- i + s // breaking thread safety
            Integer i |> gen.Return
        Gen.sized integer'
        
    type IntegerGen =
        static member Integer() =
            {new Arbitrary<Integer>() with
                override x.Generator = integer
                override x.Shrinker t = Seq.empty }

    [<Fact>]
    let ``parallelTest doesnt works with custom non threadsafe arb`` () =
        Arb.register<IntegerGen> ()
        let body i = true
        let arb = Arb.from<Integer>
        let config = { Config.Quick with MaxTest = 300000; EndSize = 300000 }
        if check arb body config then failwith "assertion failure!"

module Runner =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Swensen.Unquote

    type TestArbitrary1 =
        static member PositiveDouble() =
            Arb.Default.Float()
            |> Arb.mapFilter abs (fun t -> t >= 0.0)

    type TestArbitrary2 =
        static member NegativeDouble() =
            Arb.Default.Float()
            |> Arb.mapFilter (abs >> ((-) 0.0)) (fun t -> t <= 0.0)

    [<Property( Arbitrary=[| typeof<TestArbitrary2>; typeof<TestArbitrary1> |] )>]
    let ``should register Arbitrary instances from Config in last to first order``(underTest:float) =
        underTest <= 0.0

    [<Fact>]
    let ``should discard case with discardexception in gen``() =
        let myGen = 
            gen {
                let! a = Gen.choose(0, 4)
                return if a > 3 
                            then Prop.discard()
                            else a
            }

        let myArb = Arb.fromGen myGen
        
        Check.QuickThrowOnFailure <| Prop.forAll myArb (fun a -> a <= 3)

    [<Fact>]
    let ``should discard case with discardexception in test``() =
        Check.QuickThrowOnFailure <| (fun a -> if a > 3 then Prop.discard() else true)
        
    [<Fact>]
    let ``should replay property with one generator``() =
        let doOne(s1,s2) =
            try
                Check.One( {Config.QuickThrowOnFailure with Replay = Some <| Random.createWithSeedAndGamma (s1,s2) }, fun a -> a < 5)
                "should have failed"
            with e ->
                e.Message
        let same =
            Seq.initInfinite (fun _ -> doOne(123UL,654321UL))
            |> Seq.take(5)
            |> Seq.distinct
        1 =! Seq.length same
        "should have failed" <>! Seq.head same
        test <@ (Seq.head same).Contains "(123,654321)" @>

    [<Fact>]
    let ``should replay property with complex set of generators``() =
        let doOne(s1,s2) =
            try
                Check.One( {Config.QuickThrowOnFailure with Replay = Some <| Random.createWithSeedAndGamma (s1,s2) }, fun a (_:list<char>, _:array<int*double>) (_:DateTime) -> a < 10)
                "should have failed"
            with e ->
                e.Message
        let same =
            Seq.initInfinite (fun _ -> doOne(123UL,654321UL))
            |> Seq.take(5)
            |> Seq.distinct
        1 =! Seq.length same
        "should have failed" <>! Seq.head same
        test <@ (Seq.head same).Contains "(123,654321)" @>

    [<Property(Replay="54321,67583")>]
    let ``should pick up replay seeds from PropertyAttribute without parens``(_:int, _:string) =
        //testing the replay separately in other tests - this just checks we can run
        //this test
        ()

    [<Property(Replay="(54321,67583)")>]
    let ``should pick up replay seeds from PropertyAttribute with parens``(_:int, _:string) =
        //testing the replay separately in other tests - this just checks we can run
        //this test
        ()

    [<Fact>]
    let ``PropertyConfig combine should prepend extra Arbitrary``() =
        let original = { PropertyConfig.zero with Arbitrary = [| typeof<TestArbitrary1> |] }
        let extra    = { PropertyConfig.zero with Arbitrary = [| typeof<TestArbitrary2> |] }
        let combined = PropertyConfig.combine extra original

        combined.Arbitrary.[0] =! typeof<TestArbitrary2>

    [<Property>]
    let ``PropertyConfig combine should favor extra config``(orignalMaxTest, extraMaxTest) =
        let original = { PropertyConfig.zero with MaxTest = Some orignalMaxTest }
        let extra    = { PropertyConfig.zero with MaxTest = Some extraMaxTest }
        let combined = PropertyConfig.combine extra original

        combined.MaxTest =! Some extraMaxTest

    [<Property>]
    let ``PropertyConfig toConfig should favor specified setting``(maxTest) =
        let propertyConfig = { PropertyConfig.zero with MaxTest = Some maxTest }
        let testOutputHelper = new Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.MaxTest =! maxTest

    [<Fact>]
    let ``PropertyConfig toConfig should use defaults as a fallback``() =
        let propertyConfig = PropertyConfig.zero
        let testOutputHelper = new Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.MaxTest =! Config.Default.MaxTest

    type TypeToInstantiate() =
        [<Property>]
        member __.``Should run a property on an instance``(_:int) = ()

    [<Properties(Arbitrary = [| typeof<TestArbitrary2> |])>]
    module ModuleWithPropertiesArb =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

    [<Properties( MaxTest = 1, StartSize = 100, EndSize = 100, Replay = "01235,56789")>]
    module ModuleWithPropertiesConfig =

        [<Property>]
        let ``should use configuration from enclosing module``(x:int) =
            // checking if the generated value is always the same (-59) from "01234,56789" Replay
            x =! -4

        [<Property( Replay = "12345,67891")>]
        let ``should use configuration on method preferentially``(x:int) =
            // checking if the generated value is always the same (18) from "12345,67890" Replay
            x =! -93

module BugReproIssue195 =

    open FsCheck
    open FsCheck.Xunit
    open System

    let intStr = Arb.Default.Int32() |> Arb.toGen |> Gen.map string

    // since this used to go via from<string>, it memoized the string generator, meaning at
    // registration time for the string generator below, a duplicate key exception was thrown.
    // this was fixed by using Default.String() in StringWithoutNullChars instead, and a bunch
    // of other similar cases in the default generator was fixed as well.
    let breaksIt = Arb.Default.StringWithoutNullChars() |> ignore

    type BrokenGen = 
        static member String() = intStr |> Arb.fromGen

    [<Property(Arbitrary = [| typeof<BrokenGen> |])>]
    let ``broken`` (s : String) = s |> ignore
    
    
// see https://github.com/fscheck/FsCheck/issues/344
// Consider a test, where the first iteration failed, but all subsequent shrinks passed.
// Each successfull shrink caused the stackframe to grow by 2-3 frames, causing a stackoverflow.
module BugReproIssue344 =
    
    open FsCheck
    open global.Xunit

    open System.Diagnostics
    open System.Threading

    type IntWrapper = IntWrapper of int
      with
        member x.I =
            let (IntWrapper i) = x
            i

    type MyGenerators =
        static member FilterModel =
            { new Arbitrary<IntWrapper>() with
                // start with 1 ...
                override __.Generator =
                    Gen.constant (IntWrapper 1)
                // ... and count upwards with each shrink
                override __.Shrinker _ =
                    Seq.initInfinite (fun i -> IntWrapper(i + 2))
            }

    [<Fact>]
    let ``Shrinks dont cause a stackoverflow``() =

        // so this is really ugly and hacky, but I don't know how to better signal abortion from inside the callback.
        // throwing an exception doesn't work, since it then just assumes the test failed, and tries to shrink again ...
        let mutable tooManyFrames = false
            
        // run this on another thread, and abort it manually
        let thread2 = Thread(fun () ->
            let config = { Config.Quick with Arbitrary = [ typeof<MyGenerators> ]; ParallelRunConfig = None }
            Check.One(config, fun (x:IntWrapper) -> 
                // fail the first iteration
                if x.I = 1 then
                    false
                else
                    if x.I >= 200 then
                        // after 200 iterations, check the frame count and abort
                        let st = new StackTrace()
                        tooManyFrames <- st.FrameCount > 200
                        Thread.CurrentThread.Abort()
                    true))
        thread2.Start()
        thread2.Join()
        if tooManyFrames then failwith "too many frames, possible stackoverflow detected"


// Compiler warning FS0044 occurs when a construct is deprecated.
// This warning suppression has to sit in the end of the file, because once a
// warning type is suppressed in a file, it can't be turned back on. There's a
// feature request for that, though: 
// https://fslang.uservoice.com/forums/245727-f-language/suggestions/6085102-allow-f-compiler-directives-like-nowarn-to-span
#nowarn"44"

module Deprecated =

    open FsCheck.Xunit
    open Runner
    
    [<Arbitrary(typeof<TestArbitrary2>)>]
    module ModuleWithArbitrary =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0