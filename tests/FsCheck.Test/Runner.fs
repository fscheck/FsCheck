namespace FsCheck.Test

open FsCheck
open FsCheck.FSharp

module RunnerHelper =

    open System.Linq

    let cmpTestData = function 
        { NumberOfTests = nt1; NumberOfShrinks = ns1; Stamps = sx1; Labels = lx1 }, 
        { NumberOfTests = nt2; NumberOfShrinks = ns2; Stamps = sx2; Labels = lx2 } -> 
            nt1 = nt2 && ns1 = ns2 && Enumerable.SequenceEqual (sx1, sx2) && Enumerable.SequenceEqual (lx1, lx2)
    let cmpOutcome = function
        | Outcome.Failed ex1, Outcome.Failed ex2 -> ex1.Message.Equals ex2.Message
        | Outcome.Passed, Outcome.Passed -> true
        | Outcome.Rejected, Outcome.Rejected -> true
        | _, _ -> false  

module RunnerInternals =
    open System
    open global.Xunit
    open System.Linq
    open RunnerHelper

    type ProbeRunner () =
        let mutable result = None
        let sink :Collections.Generic.List<obj> = ResizeArray ()

        member __.Result = result.Value
        member __.Sink = sink
        member __.IsSame (pr:ProbeRunner) =
            let resultEq = 
                match pr.Result, __.Result with
                | TestResult.Passed (td1, so1), TestResult.Passed (td2, so2) -> 
                    so1 = so2 && cmpTestData (td1, td2)
                | TestResult.Failed (td1, oa1, sa1, o1, r1, rr1, s1), TestResult.Failed (td2, oa2, sa2, o2, r2, rr2, s2) -> 
                    cmpTestData (td1, td2) && 
                    cmpOutcome (o1, o2) && 
                    Enumerable.SequenceEqual (oa1, oa2) && 
                    Enumerable.SequenceEqual (sa1, sa2) &&
                    r1 = r2 &&
                    rr1 = rr2 &&
                    s1 = s2
                | TestResult.Exhausted td1, TestResult.Exhausted td2 ->
                    cmpTestData (td1, td2)
                | a1, b1-> false
            resultEq && Enumerable.SequenceEqual (pr.Sink, sink)
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


    let check arb body (config:Config) =
        let p = Prop.forAll arb body
        let seed = Random.Create ()
        let replay = { Rnd = seed; Size = None } |> Some
        printfn "seed %A" seed
        let runner1 = ProbeRunner () 
        Runner.check (config.WithReplay(replay).WithRunner(runner1).WithParallelRunConfig(Some { MaxDegreeOfParallelism = Environment.ProcessorCount })) p
        let runner2 = ProbeRunner () 
        Runner.check (config.WithReplay(replay).WithRunner(runner2).WithParallelRunConfig(None)) p
        runner1.IsSame runner2
            
    [<Fact>]
    let ``parallelTest produces same sequence as test on success`` () =
        let body i = true
        let arb = ArbMap.defaults |> ArbMap.arbitrary<int>
        let config = Config.Quick.WithMaxTest(30000).WithEndSize(30000)
        if not <| check arb body config then failwith "assertion failure!"

    [<Fact>]
    let ``parallelTest produces same sequence as test on failure`` () =
        let body i = i < 999
        let arb = ArbMap.defaults |> ArbMap.arbitrary<int>
        let config = Config.Quick.WithMaxTest(30000).WithEndSize(30000)
        if not <| check arb body config then failwith "assertion failure!"
            
    [<Fact>]
    let ``parallelTest produces same sequence as test on discard`` () =
        let body _ = true
        let arb = Arb.fromGen (Gen.constant 1 |> Gen.map (fun _ -> Prop.discard ()))
        let config = Config.Quick.WithMaxTest(30000).WithMaxRejected(100000).WithEndSize(30000)
        if not <| check arb body config then failwith "assertion failure!"
    
    type Tree = Leaf of int | Branch of Tree * Tree
    
    let tree =
        let rec tree' s = 
            match s with
            | 0 -> ArbMap.defaults |> ArbMap.generate<int> |> Gen.map Leaf
            | n when n>0 -> 
                let subtree = tree' (n/2)
                Gen.oneof [ ArbMap.defaults |> ArbMap.generate<int> |> Gen.map Leaf
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
        let arb = Arb.list <| Arb.fromGen(tree)
        let body (xs:list<Tree>) = List.rev(List.rev xs) = xs // fsharplint:disable-line Hints
        let config = Config.Quick.WithMaxTest(3000).WithEndSize(3000).WithArbitrary([typeof<TreeGen>])
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
    let ``parallelTest does not work with custom non threadsafe arb`` () =
        let arbs = ArbMap.defaults |> ArbMap.mergeWith<IntegerGen>
        let body i = true
        let arb = arbs.ArbFor<Integer>()
        let config = Config.Quick.WithMaxTest(300000).WithEndSize(300000).WithArbitrary([typeof<IntegerGen>])
        if check arb body config then failwith "assertion failure!"
    
    type Input =
        | String of string
        | Int of int

    type NonNullStringArb =
        static member NonNullString () =
            ArbMap.defaults.ArbFor<String>()
            |> Arb.filter(isNull>>not)

    [<Fact>]
    let ``parallelTest propagates Arb register to threads``() =
        let prop = function
            | String s -> not (isNull s)
            | Int _ -> true

        let config = Config.QuickThrowOnFailure
                           .WithMaxTest(100)
                           .WithParallelRunConfig(Some { MaxDegreeOfParallelism = Environment.ProcessorCount })
                           .WithArbitrary([typeof<NonNullStringArb>])

        Check.One(config, prop)   
        
    [<Fact>]
    let ```QuickThrowOnFailure should surface exception message``() =
        try 
            Check.QuickThrowOnFailure(fun i -> if abs i < 20 then true else failwith "bespoke error message")
            do failwith "should have thrown an exception!"
        with e ->
            Helpers.assertTrue <| e.Message.Contains "with exception:"
            Helpers.assertTrue <| e.Message.Contains "bespoke error message"

module Runner =
    open RunnerHelper
    open System
    open global.Xunit
    open FsCheck.Xunit
    open System.Linq
    open Swensen.Unquote

    type PositiveDoublesOnly =
        static member PositiveDouble() =
            ArbMap.defaults.ArbFor<float>()
            |> Arb.mapFilter abs (fun t -> t >= 0.0)

    type NegativeDoublesOnly =
        static member NegativeDouble() =
            ArbMap.defaults.ArbFor<float>()
            |> Arb.mapFilter (abs >> ((-) 0.0)) (fun t -> t <= 0.0)

    [<Property( Arbitrary=[| typeof<NegativeDoublesOnly>; typeof<PositiveDoublesOnly> |] )>]
    let ``should register Arbitrary instances from Config in last to first order``(underTest:float) =
        underTest <= 0.0

    type NegativeDoublesOnlyAsProperty =
        static member NegativeDouble =
            NegativeDoublesOnly.NegativeDouble()

    [<Property( Arbitrary=[| typeof<NegativeDoublesOnlyAsProperty> |] )>]
    let ``should register Arbitrary instances defined as properties``(underTest:float) =
        underTest <= 0.0

    type NegativeDoublesOnlyAsAutoProperty() =
        static member val NegativeDouble  =
            NegativeDoublesOnly.NegativeDouble()

    [<Property( Arbitrary=[| typeof<NegativeDoublesOnlyAsAutoProperty> |] )>]
    let ``should register Arbitrary instances defined as auto-implemented properties ``(underTest:float) =
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
    let ``should show control characters as character codes``() =
        let stringWithControlChar = "\r\n\t1234\001 fadf"
        let result = Runner.argumentsToString [stringWithControlChar]
        test <@ result.StartsWith("\"\r\n\t1234\\001 fadf") @>

    [<Fact>]
    let ``should show control characters as character codes for strings nested in other types``() =
        let stringWithControlChar = Some (Choice1Of2 "1234\001 fadf")
        let result = Runner.argumentsToString [stringWithControlChar]
        test <@ result.StartsWith("Some (Choice1Of2 \"1234\\001 fadf") @>

    [<Fact>]
    let ``should replay property with one generator``() =
        let doOne(s1,s2) =
            try
                Check.One(
                    Config.QuickThrowOnFailure.WithReplay(Some <| {Rnd = Random.CreateWithSeedAndGamma (s1,s2); Size = None}),
                    fun a -> a < 5
                )
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
                Check.One( 
                    Config.QuickThrowOnFailure.WithReplay(Some {Rnd = Random.CreateWithSeedAndGamma (s1,s2); Size = None}),
                    fun a (_:list<char>, _:array<int*double>) (_:DateTime) -> a < 10
                )
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

    
    //type Integer = Integer of int
    type UInteger = UInteger of uint32
        
    //type IntegerGen =
    //    static member Integer() =
    //        {new Arbitrary<Integer>() with
    //            override x.Generator = Arb.generate<int> Arb.defaults |> Gen.map Integer
    //            override x.Shrinker t = Seq.empty }

    type UIntegerGen =
            static member UInteger() =
                {new Arbitrary<UInteger>() with
                    override x.Generator = ArbMap.defaults.ArbFor<uint32>().Generator |> Gen.map UInteger
                    override x.Shrinker t = Seq.empty }
    
    //Arb.register<UIntegerGen> ()
    //Arb.register<IntegerGen> ()

    type ProbeRunner () =
        let mutable result = None
        member __.TestData () = match result.Value with
                                | TestResult.Passed (t, _) -> None
                                | TestResult.Failed (t, oa, sa, o, r, rr, s) -> Some (t, oa, sa, o, r, rr, s)
                                | TestResult.Exhausted t -> None
        interface IRunner with
            override __.OnStartFixture _ = ()
            override __.OnArguments (_, _, _) = ()
            override __.OnShrink (_, _) = ()
            override __.OnFinished (_, testResult) = 
                result <- Some testResult

    let cmpTestData = function 
        { NumberOfShrinks = ns1; Stamps = sx1; Labels = lx1 }, 
        { NumberOfShrinks = ns2; Stamps = sx2; Labels = lx2 } -> 
           ns1 = ns2 && Enumerable.SequenceEqual (sx1, sx2) && Enumerable.SequenceEqual (lx1, lx2)

    [<Property(StartSize = 10, EndSize = 100000, MaxTest = 200, MaxRejected = 0, Arbitrary=[|typeof<UIntegerGen>|])>]
    let ``should fast-forward properly on failing funcs``(f :(int -> bool), (UInteger a)) =
        let runs = Convert.ToInt32(Math.Min(a,100000u)) + 1
        let rnd = Random.Create ()
        let runner = ProbeRunner ()
        let cfg = 
            Config.Quick
                    .WithReplay(Some {Rnd = rnd; Size = None})
                    .WithMaxTest(runs)
                    .WithStartSize(0)
                    .WithEndSize(runs)
                    .WithMaxRejected(1000)
                    .WithRunner(runner)

        Check.One (cfg, f)
        match runner.TestData () with
        | None -> true
        | Some (td1, oa1, sa1, o1, r1, rr1, s1) ->
            Check.One (cfg.WithReplay(Some {Rnd = rr1; Size = Some s1}), f)
            match runner.TestData () with
            | None -> false
            | Some (td2, oa2, sa2, o2, r2, rr2, s2) ->
                cmpTestData (td1, td2) && 
                cmpOutcome (o1, o2) && 
                Enumerable.SequenceEqual (oa1, oa2) && 
                Enumerable.SequenceEqual (sa1, sa2) &&
                rr1 = r2 &&
                rr1 = rr2 &&
                s1 = s2 

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
        let original = { PropertyConfig.zero with Arbitrary = [| typeof<PositiveDoublesOnly> |] }
        let extra    = { PropertyConfig.zero with Arbitrary = [| typeof<NegativeDoublesOnly> |] }
        let combined = PropertyConfig.combine extra original

        combined.Arbitrary.[0] =! typeof<NegativeDoublesOnly>

    [<Property>]
    let ``PropertyConfig combine should favor extra config``(orignalMaxTest, extraMaxTest) =
        let original = { PropertyConfig.zero with MaxTest = Some orignalMaxTest }
        let extra    = { PropertyConfig.zero with MaxTest = Some extraMaxTest }
        let combined = PropertyConfig.combine extra original

        combined.MaxTest =! Some extraMaxTest

    [<Property>]
    let ``PropertyConfig toConfig should favor specified setting``(maxTest) =
        let propertyConfig = { PropertyConfig.zero with MaxTest = Some maxTest }
        let testOutputHelper = Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.MaxTest =! maxTest

    [<Fact>]
    let ``PropertyConfig toConfig should use defaults as a fallback``() =
        let propertyConfig = PropertyConfig.zero
        let testOutputHelper = Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.MaxTest =! Config.Default.MaxTest

    [<Property>]
    let ``Replay should pick fast-forward``(size :int) =
        let size = Math.Abs size
        let propertyConfig = { PropertyConfig.zero with Replay = Some <| sprintf "(01234,56789,%i)" size }
        let testOutputHelper = Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.Replay =! (Some {Rnd = Random.CreateWithSeedAndGamma (01234UL,56789UL); Size = Some size})

    [<Fact>]
    let ``Replay with no fast-forward``() =
        let propertyConfig = { PropertyConfig.zero with Replay = Some <| sprintf "(01234,56789)" }
        let testOutputHelper = Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.Replay =! (Some {Rnd = Random.CreateWithSeedAndGamma (01234UL,56789UL); Size = None})

    type TypeToInstantiate() =
        [<Property>]
        member __.``Should run a property on an instance``(_:int) = ()

    [<Properties(Arbitrary = [| typeof<NegativeDoublesOnly> |])>]
    module ModuleWithPropertiesArb =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        module NestedModuleWithPropertiesArb =
        
            [<Property>]
            let ``should use Arb instances from enclosing enclosing module``(underTest:float) =
                underTest <= 0.0
        
        [<Properties( Arbitrary=[| typeof<PositiveDoublesOnly> |])>]
        module NestedModuleWithPropertiesConfig =
                
            [<Property>]
            let ``should use Arb instances from closest enclosing module``(underTest:float) =
                underTest >= 0.0

        [<Property( Arbitrary=[| typeof<PositiveDoublesOnly> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

    [<Properties( MaxTest = 1, StartSize = 100, EndSize = 100, Replay = "(01235,56789)")>]
    module ModuleWithPropertiesConfig =

        [<Property>]
        let ``should use configuration from enclosing module``(x:int) =
            // checking if the generated value is always the same (-59) from "01234,56789" Replay
            x =! -4

        module NestedModuleWithoutPropertiesConfig =

            [<Property>]
            let ``should use configuration from enclosing enclosing module``(x:int) =
                // checking if the generated value is always the same (-59) from "01234,56789" Replay
                x =! -4

        [<Properties( MaxTest = 1, StartSize = 100, EndSize = 100, Replay = "12345,67891")>]
        module NestedModuleWithPropertiesConfig =
        
            [<Property>]
            let ``should use configuration from closest enclosing module``(x:int) =
                /// checking if the generated value is always the same (18) from "12345,67891" Replay
               x =! -93

        [<Property( Replay = "12345,67891")>]
        let ``should use configuration on method preferentially``(x:int) =
            // checking if the generated value is always the same (18) from "12345,67890" Replay
            x =! -93

module BugReproIssue195 =

    open FsCheck
    open FsCheck.Xunit
    open System

    let intStr = ArbMap.defaults |> ArbMap.generate<int> |> Gen.map string

    // since this used to go via from<string>, it memoized the string generator, meaning at
    // registration time for the string generator below, a duplicate key exception was thrown.
    // this was fixed by using Default.String() in StringWithoutNullChars instead, and a bunch
    // of other similar cases in the default generator was fixed as well.
    let breaksIt = ArbMap.defaults.ArbFor<StringNoNulls>() |> ignore

    type BrokenGen = 
        static member String() = intStr |> Arb.fromGen

    [<Property(Arbitrary = [| typeof<BrokenGen> |])>]
    let ``broken`` (s : String) = s |> ignore
    
    
// see https://github.com/fscheck/FsCheck/issues/344
// Consider a test, where the first iteration failed, but all subsequent shrinks passed.
// Each successful shrink caused the stackframe to grow by 2-3 frames, causing a stackoverflow.
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
                    Seq.initInfinite (fun i -> IntWrapper(i + 2)) |> Seq.take 200
            }
    [<Fact>]
    let ``Shrinks dont cause a stackoverflow``() =

        // so this is really ugly and hacky, but I don't know how to better signal abortion from inside the callback.
        // throwing an exception doesn't work, since it then just assumes the test failed, and tries to shrink again ...
        let mutable tooManyFrames = false
            
        // run this on another thread, and abort it manually
        let thread2 = Thread(fun () ->
            let config = Config.Quick.WithArbitrary([typeof<MyGenerators>]).WithParallelRunConfig(None)
            Check.One(config, fun (x:IntWrapper) -> 
                // fail the first iteration
                if x.I = 1 then
                    false
                else
                    if x.I >= 200 then
                        // after 200 iterations, check the frame count
                        let st = StackTrace()
                        tooManyFrames <- st.FrameCount > 200
                    true))
        thread2.Start()
        thread2.Join()
        if tooManyFrames then failwith "too many frames, possible stackoverflow detected"

module Override =
    open System
    open FsCheck
    open global.Xunit

    type Calc = { Float: float }

    type Arbitraries =
        static member Float() = 
            ArbMap.defaults.ArbFor<NormalFloat>() |> Arb.convert float NormalFloat
        //static member Calc() = Arb.Default.Derive<Calc>()

    [<Fact>]
    let ``should use override in same Arbitrary class``() =
//        Check.One(Config.QuickThrowOnFailure, fun (_:Calc) -> true)
        Check.One(Config.QuickThrowOnFailure.WithArbitrary([ typeof<Arbitraries> ]),
             fun (calc:Calc) -> not (Double.IsNaN calc.Float || Double.IsInfinity calc.Float || calc.Float = Double.Epsilon || calc.Float = Double.MinValue || calc.Float = Double.MaxValue))

// see https://github.com/fscheck/FsCheck/issues/514
// Dispose not called
module BugReproIssue514 =
    open System
    open System.Threading
    open FsCheck
    open FsCheck.Xunit
    open global.Xunit
    open Xunit.Sdk

    type TestMessageBus() =
        interface IMessageBus with
            member _.QueueMessage _ = true
            member _.Dispose() = ()

    let mutable disposed = false

    type DisposableTestClass() =

        [<Property>]
        member _.FakeTest (x:int) =
            Check.One(Config.Quick, true)       

        interface IDisposable with
            member _.Dispose() = 
                disposed <- true

    [<Property>]
    let ``should call Dispose on classes inheriting from IDisposable`` () =
            let methodInfo = typeof<DisposableTestClass>.GetMethod("FakeTest") |> ReflectionMethodInfo
            let typeInfo = typeof<DisposableTestClass> |> ReflectionTypeInfo
            let assemblyInfo = typeof<DisposableTestClass>.Assembly |> ReflectionAssemblyInfo |> TestAssembly
            let testCollection = TestCollection(assemblyInfo, typeInfo, typeof<DisposableTestClass>.Name)
            let testClass = TestClass(testCollection, typeInfo)
            let testMethod = TestMethod(testClass, methodInfo)
            let testCase = new PropertyTestCase(null, TestMethodDisplay.ClassAndMethod, TestMethodDisplayOptions.None, testMethod)
            testCase.RunAsync(null, new TestMessageBus(), [||], ExceptionAggregator(), new CancellationTokenSource()) |> Async.AwaitTask |> ignore
            Check.One(Config.Quick, disposed)


module ShrinkingMutatedTypes =
    open global.Xunit
    open Swensen.Unquote
    open FsCheck


    type Member () =
        member val Name = "" with get, set

    
    let mutateMember (m:Member)  =
        m.Name <- "1"
        m.Name = "0"

    [<Fact>]
    let ``should not loop infinitely when shrinking mutated value``() =
        raisesWith <@ Check.QuickThrowOnFailure(mutateMember) @> (fun e -> <@ e.Message.Contains("5000 shrinks") @>)
            
        
module BugReproIssue583 =
    module Common =
        open FsCheck

        type BaseGenerator =
            static member Strings() =
                {new Arbitrary<string>() with
                    override x.Generator = Gen.constant "FsCheck"
                }

    module MyTests =
        open FsCheck
        open FsCheck.Xunit
        open Common

        type DerivedGenerator =
            inherit BaseGenerator
            
            static member Ints() =
                {new Arbitrary<int32>() with
                    override x.Generator = Gen.constant 5
                }

        
        [<Property(Arbitrary = [| typeof<DerivedGenerator> |])>]
        let ``should use the inherited generator`` (str: string) =
            str = "FsCheck"
        
        [<Property(Arbitrary = [| typeof<DerivedGenerator> |])>]
        let ``should use the derived generator`` (i: int32) =
            i = 5
