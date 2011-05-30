
namespace FsCheck

open System
open Xunit
open Xunit.Sdk


type PropertyFailedException(testResult:FsCheck.TestResult) = 
    inherit AssertException(sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" testResult))

//can not be an anonymous type because of let mutable.
type private XunitRunner() =
    let mutable result = None
    member x.Result = result.Value
    interface IRunner with
        override x.OnStartFixture t = ()
        override x.OnArguments (ntest,args, every) = 
            printf "%s" (every ntest args)
        override x.OnShrink(args, everyShrink) = 
            printf "%s" (everyShrink args)
        override x.OnFinished(name,testResult) = 
            result <- Some testResult

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type PropertyAttribute() =
    inherit FactAttribute()
    let mutable maxTest = Config.Default.MaxTest
    let mutable maxFail = Config.Default.MaxFail
    //StdGen won't work as argument to attribute. First do away with Random then, so we have just one seed?
//    let mutable replay = 
//        match Config.Default.Replay with
//        | None -> null
//        | Some stdgen ->   
    let mutable startSize = Config.Default.StartSize
    let mutable endSize = Config.Default.EndSize
    let mutable verbose = false

    member x.MaxTest with get() = maxTest and set(v) = maxTest <- v
    member x.MaxFail with get() = maxFail and set(v) = maxFail <- v
    member x.StartSize with get() = startSize and set(v) = startSize <- v
    member x.EndSize with get() = endSize and set(v) = endSize <- v
    member x.Verbose with get() = verbose and set(v) = verbose <- v

    override x.EnumerateTestCommands(methodInfo:IMethodInfo) :seq<ITestCommand> = 
        { new TestCommand(methodInfo, null, 0) with
            override this.Execute(testClass:obj) : MethodResult = 
                let xunitRunner = XunitRunner()
                let config = 
                    {Config.Default with
                        MaxTest = maxTest
                        MaxFail = maxFail
                        StartSize = startSize
                        EndSize = endSize
                        Every = if verbose then Config.Verbose.Every else Config.Quick.Every
                        EveryShrink = if verbose then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                        Runner = xunitRunner
                    }
                Check.Method(config, methodInfo.MethodInfo)
                match xunitRunner.Result with
                | TestResult.True _ -> 
                    upcast new PassedResult(methodInfo,this.DisplayName)
                | Exhausted testdata as testResult -> 
                    upcast new FailedResult(methodInfo,PropertyFailedException(testResult), this.DisplayName)
                | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed) as testResult -> 
                    upcast new FailedResult(methodInfo, PropertyFailedException(testResult), this.DisplayName)
        } :> ITestCommand
        |> Seq.singleton