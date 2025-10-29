namespace FsCheck.NUnit

open System
open System.Threading
open System.Reflection

open FsCheck

open NUnit.Framework
open NUnit.Framework.Interfaces
open NUnit.Framework.Internal

//can not be an anonymous type because of let mutable.
type private NunitRunner() =
    let mutable result = None
    member __.Result = result.Value
    interface IRunner with
        override __.OnStartFixture _ = ()
        override __.OnArguments (ntest,args, every) =
            let argsForOutput = every ntest args
            // Only write the args if there's something to write.
            // Although it may seem harmless to write an empty string, writing anything at this stage seems to
            // trigger NUnit's formatting system, so that it adds a 'header' for the test in the output. That
            // doesn't look pretty in the cases where it turns out that there's truly nothing to write (e.g.
            // when QuietOnSuccess is true, and the Property passed.
            if not (String.IsNullOrWhiteSpace argsForOutput) then
                printfn "%s" argsForOutput
        override __.OnShrink(args, everyShrink) =
            printfn "%s" (everyShrink args)
        override __.OnFinished(_,testResult) =
            result <- Some testResult

type internal PropertyConfig =
    { MaxTest        : Option<int>
      MaxRejected    : Option<int>
      Replay         : Option<string>
      Parallelism    : Option<int>
      StartSize      : Option<int>
      EndSize        : Option<int>
      Verbose        : Option<bool>
      QuietOnSuccess : Option<bool>
      Arbitrary      : Type[] }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal PropertyConfig = 
    let orElse y = function
        | Some x -> Some x
        | None   -> y

    let orDefault x y = defaultArg y x

    let zero =
        { MaxTest        = None
          MaxRejected    = None
          Replay         = None
          Parallelism    = None
          StartSize      = None
          EndSize        = None
          Verbose        = None
          QuietOnSuccess = None
          Arbitrary      = [||] }

    let combine extra original =
        { MaxTest        = extra.MaxTest        |> orElse original.MaxTest
          MaxRejected    = extra.MaxRejected    |> orElse original.MaxRejected
          Replay         = extra.Replay         |> orElse original.Replay
          Parallelism    = extra.Parallelism    |> orElse original.Parallelism
          StartSize      = extra.StartSize      |> orElse original.StartSize
          EndSize        = extra.EndSize        |> orElse original.EndSize
          Verbose        = extra.Verbose        |> orElse original.Verbose
          QuietOnSuccess = extra.QuietOnSuccess |> orElse original.QuietOnSuccess
          Arbitrary      = Array.append extra.Arbitrary original.Arbitrary }

    let parseReplay (str: string) =
        //if someone sets this, we want it to throw if it fails
        let split = str.Trim('(',')').Split([|","|], StringSplitOptions.RemoveEmptyEntries)
        let seed = UInt64.Parse(split.[0])
        let gamma = UInt64.Parse(split.[1])
        let size = if split.Length = 3 then Some <| Convert.ToInt32(UInt32.Parse(split.[2])) else None
        { Rnd = Rnd (seed,gamma); Size = size }

    let toConfig runner propertyConfig =
        Config.Default
              .WithReplay(
                  propertyConfig.Replay
                  |> Option.map parseReplay
                  |> orElse Config.Default.Replay
              )
              .WithParallelRunConfig(
                  propertyConfig.Parallelism
                  |> Option.map (fun i -> { MaxDegreeOfParallelism = i })
                  |> orElse Config.Default.ParallelRunConfig
              )
              .WithMaxTest(propertyConfig.MaxTest |> orDefault Config.Default.MaxTest)
              .WithMaxRejected(propertyConfig.MaxRejected |> orDefault Config.Default.MaxRejected)
              .WithStartSize(propertyConfig.StartSize |> orDefault Config.Default.StartSize)
              .WithEndSize(propertyConfig.EndSize |> orDefault Config.Default.EndSize)
              .WithQuietOnSuccess(propertyConfig.QuietOnSuccess |> orDefault Config.Default.QuietOnSuccess)
              .WithArbitrary(Seq.toList propertyConfig.Arbitrary)
              .WithRunner(runner)
              .WithEvery(
                  if propertyConfig.Verbose |> Option.exists id then 
                      Config.Verbose.Every
                  else
                      Config.Quick.Every
              )
              .WithEveryShrink(
                  if propertyConfig.Verbose |> Option.exists id then 
                      Config.Verbose.EveryShrink
                  else 
                      Config.Quick.EveryShrink
              )

///Run this method as an FsCheck test.
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type PropertyAttribute() =
    inherit TestAttribute()

    let mutable config = PropertyConfig.zero
    let mutable replay = null
    let mutable parallelism = -1
    let mutable maxTest = -1
    let mutable maxFail = -1
    let mutable startSize = -1
    let mutable endSize = -1
    let mutable verbose = false
    let mutable arbitrary = [||]
    let mutable quietOnSuccess = false

    ///If set, the seed to use to start testing. Allows reproduction of previous runs. You can just paste
    ///the tuple from the output window, e.g. 12344,12312 or (123,123).
    member __.Replay with get() = replay and set(v) = replay <- v; config <- {config with Replay = if String.IsNullOrEmpty v then None else Some v}
    ///If set, run tests in parallel. Useful for Task/async related work and heavy number crunching
    ///Environment.ProcessorCount have been found to be useful default.
    member __.Parallelism with get() = parallelism and set(v) = parallelism <- v; config <- {config with Parallelism = Some v}
    ///The maximum number of tests that are run.
    member __.MaxTest with get() = maxTest and set(v) = maxTest <- v; config <- {config with MaxTest = Some v}
    ///The maximum number of tests where values are rejected, e.g. as the result of ==>
    member __.MaxFail with get() = maxFail and set(v) = maxFail <- v; config <- {config with MaxRejected = Some v}
    ///The size to use for the first test.
    member __.StartSize with get() = startSize and set(v) = startSize <- v; config <- {config with StartSize = Some v}
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member __.EndSize with get() = endSize and set(v) = endSize <- v; config <- {config with EndSize = Some v}
    ///Output all generated arguments.
    member __.Verbose with get() = verbose and set(v) = verbose <- v; config <- {config with Verbose = Some v}
    ///The Arbitrary instances to use for this test method. The Arbitrary instances
    ///are merged in back to front order i.e. instances for the same generated type
    ///at the front of the array will override those at the back.
    member __.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v; config <- {config with Arbitrary = v}
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member __.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v; config <- {config with QuietOnSuccess = Some v}

    member internal __.Config = config

    interface ISimpleTestBuilder with
        override __.BuildFrom(mi, suite) =
            let method = FsCheckTestMethod(mi, suite) :> TestMethod

            mi.GetCustomAttributes<CategoryAttribute>(true) 
            |> Array.iter (fun cattr -> cattr.ApplyToTest method)

            mi.GetCustomAttributes<IgnoreAttribute>(true)
            |> Array.iter (fun cattr -> cattr.ApplyToTest method)

            method

    interface IWrapTestMethod with
        override __.Wrap command:Internal.Commands.TestCommand =
            {new Internal.Commands.TestCommand(command.Test) with
                override __.Execute context = match command.Test with
                                              | :? FsCheckTestMethod as testMethod -> testMethod.RunTest(context)
                                              | _ -> command.Execute(context) }

and
    ///Set common configuration for all properties within this class or module
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Assembly, AllowMultiple = false)>]
    PropertiesAttribute() =
    inherit PropertyAttribute()

        interface IApplyToTest with
            override __.ApplyToTest(test) = ()

and FsCheckTestMethod(mi : IMethodInfo, parentSuite : Test) =
    inherit TestMethod(mi, parentSuite)

    member x.RunTest context =
        let testResult = x.MakeTestResult()
        TestExecutionContext.CurrentContext.CurrentResult <- testResult
        try
            try
                use _testContext = new TestExecutionContext.IsolatedContext()
                x.RunSetUp()
                x.RunTestCase context testResult
            with
                | ex -> x.HandleException ex testResult FailureSite.SetUp
        finally
            x.RunTearDown testResult
        testResult

    member private x.RunSetUp() =
        if not (isNull x.SetUpMethods) then
            x.SetUpMethods |> Array.iter x.InvokeMethodIgnore

    member private x.RunTearDown testResult =
        try
            if not (isNull x.TearDownMethods) then
                x.TearDownMethods
                |> Array.rev
                |> Array.iter x.InvokeMethodIgnore
        with
            | ex ->
                testResult.RecordTearDownException(x.FilterException ex)

    member private x.InvokeMethodIgnore mi =
        mi.Invoke(if mi.IsStatic then null else x.Fixture) |> ignore

    member private __.FilterException ex =
        match ex with
        | :? NUnitException as nue when not (isNull nue.InnerException) -> nue.InnerException
        | _ -> ex

    member private x.RunTestCase context testResult =
        try
            x.RunTestMethod context testResult
        with
            | ex -> x.HandleException ex testResult FailureSite.Test

    member private x.GetFsCheckPropertyAttribute() =
        x.Method.GetCustomAttributes<PropertyAttribute> false
        |> Seq.head 

    member private x.Init(runner) =
        let combineAttributes (configs: (PropertyConfig option) list) =
            configs
            |> List.choose id
            |> List.reduce(fun higherLevelAttribute lowerLevelAttribute ->
                PropertyConfig.combine lowerLevelAttribute higherLevelAttribute)

        let getPropertiesOnDeclaringClasses (testClass: ITypeInfo) =
            [   let mutable current: Type = testClass.Type
                while not (isNull current) do
                    yield current.GetTypeInfo().GetCustomAttributes<PropertiesAttribute>()
                          |> Seq.tryHead
                          |> Option.map (fun attr -> attr.Config)
                    current <- current.DeclaringType]
            |> List.rev

        let config = combineAttributes [
              yield x.Method.TypeInfo.Assembly.GetCustomAttributes<PropertiesAttribute>() |> Seq.tryHead |> Option.map (fun t -> t.Config)
              yield! getPropertiesOnDeclaringClasses x.Method.TypeInfo
              yield x.GetFsCheckPropertyAttribute().Config |> Some]
        
        { config with Arbitrary = config.Arbitrary }
        |> PropertyConfig.toConfig runner

    member private x.HandleException ex testResult failureSite =
        match ex with
        | :? ThreadAbortException -> () // old code called obsolete Thread.ResetAbort() which is no longer supported on net core and 5+ and above, but those won't ever throw ThreadAbortException either. 
                                        // https://learn.microsoft.com/en-us/dotnet/api/system.threading.threadabortexception?view=net-8.0
        | _ -> ()
        testResult.RecordException(x.FilterException <| ex, failureSite)

    member private x.RunTestMethod context testResult =
        let testRunner = NunitRunner()
        let config = x.Init(testRunner)

        let target = if not (isNull x.Fixture) then Some x.Fixture
                     elif x.Method.MethodInfo.IsStatic then None
                     else Some context.TestObject
        Check.Method(config, x.Method.MethodInfo, ?target = target)

        let rec (|NonFailingNUnitResultStateException|_|) (exn : exn) =
            match exn with
            | :? ResultStateException as e when e.ResultState.Status <> TestStatus.Failed -> Some e
            | :? AggregateException as e -> (|NonFailingNUnitResultStateException|_|) e.InnerException
            | _ -> None

        match testRunner.Result with
        | TestResult.Passed _ ->
            if not config.QuietOnSuccess then
                printfn "%s" (Runner.onFinishedToString "" testRunner.Result)
            testResult.SetResult(ResultState(TestStatus.Passed))
        | TestResult.Exhausted _ ->
            let msg = sprintf "Exhausted: %s" (Runner.onFinishedToString "" testRunner.Result)
            testResult.SetResult(ResultState(TestStatus.Failed, msg), msg)
        | TestResult.Failed (_, _, _, Outcome.Failed (NonFailingNUnitResultStateException e), _, _, _) ->
            testResult.RecordException e
        | TestResult.Failed _ ->
            let msg = sprintf "%s" (Runner.onFinishedToString "" testRunner.Result)
            testResult.SetResult(ResultState(TestStatus.Failed, msg), msg)
